//! ADC module to mesure multiple current (from clamp)
use std::{fmt, time::Duration};

use rppal::i2c::{self, I2c};
use thiserror::Error;
use tokio::{
    sync::mpsc,
    task::{JoinError, JoinHandle},
    time::{interval, interval_at, Instant},
};

use crate::CardEvent;

/// AdcError representing a problem when reading a current from the ADC
#[derive(Error, Debug)]
pub enum AdcError {
    /// I2C transportation error
    #[error("Error on the I2C transport layer `{0}`")]
    TransportError(#[from] i2c::Error),
    /// Error on the I2C configuration for MCP3424
    #[error("Wrong I2C configuration for MCP3424 `{0}`")]
    WrongConfiguration(u8),
    /// Error on the I2C configuration for MCP3424
    #[error("Can't read the value from the ADC (unavailable) `{0}`")]
    MissingValue(AdcMeterConfig),
    /// Error in async operation on I2C
    #[error("Error in async operation on I2C `{0}`")]
    AsyncI2c(#[from] JoinError),
}

/// Rate period useful to know the current price
#[derive(Debug, Default, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum AdcChannel {
    /// J31 input ADC current meter
    #[default]
    CH1 = 0,
    /// J32 input ADC current meter
    CH2 = 0x20,
    /// J33 input ADC current meter
    CH3 = 0x40,
}

impl fmt::Display for AdcChannel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AdcChannel::CH1 => write!(f, "Channel 1"),
            AdcChannel::CH2 => write!(f, "Channel 2"),
            AdcChannel::CH3 => write!(f, "Channel 3"),
        }
    }
}

impl TryFrom<u8> for AdcChannel {
    type Error = AdcError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value & 0x60 {
            0 => Ok(AdcChannel::CH1),
            0x20 => Ok(AdcChannel::CH2),
            0x40 => Ok(AdcChannel::CH3),
            _ => Err(AdcError::WrongConfiguration(value)),
        }
    }
}

impl From<&AdcChannel> for u8 {
    fn from(value: &AdcChannel) -> Self {
        match value {
            AdcChannel::CH1 => 0,
            AdcChannel::CH2 => 0x20,
            AdcChannel::CH3 => 0x40,
        }
    }
}

/// Sample rate for the ADC
#[derive(Debug, Default, Copy, Clone)]
#[repr(u8)]
pub enum AdcSampleRate {
    /// 240 SPS (12 bits) (Default)
    #[default]
    Rate240,
    /// 60 SPS (14 bits)
    Rate60,
    /// 15 SPS (16 bits)
    Rate15,
    /// 3.75 SPS (18 bits)
    Rate3_7,
}

impl AdcSampleRate {
    /// Getter the time between each measurements
    /// The sample should provide a read value at this miliseconds period
    pub fn get_period_ms(&self) -> u64 {
        match self {
            AdcSampleRate::Rate240 => 10,
            AdcSampleRate::Rate60 => 20,
            AdcSampleRate::Rate15 => 70,
            AdcSampleRate::Rate3_7 => 280,
        }
    }

    /// Getter of the output value size when using this sample rate
    pub fn get_value_output_size(&self) -> usize {
        match self {
            AdcSampleRate::Rate3_7 => 4,
            _ => 3,
        }
    }

    /// Getter coeficient to get the current out of a ADC raw value
    pub fn get_current_coef(&self) -> f64 {
        match self {
            AdcSampleRate::Rate240 => 990.0,
            AdcSampleRate::Rate60 => 1160.0,
            AdcSampleRate::Rate15 => 1324.0,
            AdcSampleRate::Rate3_7 => 1490.0,
        }
    }
}

impl fmt::Display for AdcSampleRate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AdcSampleRate::Rate240 => write!(f, "240 SPS (12 bits) (Default)"),
            AdcSampleRate::Rate60 => write!(f, "60 SPS (14 bits)"),
            AdcSampleRate::Rate15 => write!(f, "15 SPS (16 bits)"),
            AdcSampleRate::Rate3_7 => write!(f, "3.75 SPS (18 bits)"),
        }
    }
}

impl From<u8> for AdcSampleRate {
    fn from(value: u8) -> Self {
        match value & 0x0C {
            0 => AdcSampleRate::Rate240,
            0x04 => AdcSampleRate::Rate60,
            0x08 => AdcSampleRate::Rate15,
            0x0C => AdcSampleRate::Rate3_7,
            _ => panic!("AdcSampleRate should never have this value"),
        }
    }
}

impl From<AdcSampleRate> for u8 {
    fn from(value: AdcSampleRate) -> Self {
        match value {
            AdcSampleRate::Rate240 => 0,
            AdcSampleRate::Rate60 => 0x04,
            AdcSampleRate::Rate15 => 0x08,
            AdcSampleRate::Rate3_7 => 0x0C,
        }
    }
}

/// PGA Gain for the ADC
#[derive(Debug, Default, Copy, Clone)]
#[repr(u8)]
pub enum AdcPGAGain {
    /// Gain of x1 on the read value
    #[default]
    X1,
    /// Gain of x2 on the read value
    X2,
    /// Gain of x4 on the read value
    X4,
    /// Gain of x8 on the read value
    X8,
}

impl AdcPGAGain {
    /// Getter of the gain value
    pub fn get_gain(&self) -> f64 {
        match self {
            AdcPGAGain::X1 => 1.0,
            AdcPGAGain::X2 => 2.0,
            AdcPGAGain::X4 => 4.0,
            AdcPGAGain::X8 => 8.0,
        }
    }
}

impl fmt::Display for AdcPGAGain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AdcPGAGain::X1 => write!(f, "x1"),
            AdcPGAGain::X2 => write!(f, "x2"),
            AdcPGAGain::X4 => write!(f, "x4"),
            AdcPGAGain::X8 => write!(f, "x8"),
        }
    }
}

impl From<u8> for AdcPGAGain {
    fn from(value: u8) -> Self {
        match value & 0x03 {
            0 => AdcPGAGain::X1,
            0x01 => AdcPGAGain::X2,
            0x02 => AdcPGAGain::X4,
            0x03 => AdcPGAGain::X8,
            _ => panic!("AdcPGAGain should never have this value"),
        }
    }
}

impl From<AdcPGAGain> for u8 {
    fn from(value: AdcPGAGain) -> Self {
        match value {
            AdcPGAGain::X1 => 0,
            AdcPGAGain::X2 => 0x01,
            AdcPGAGain::X4 => 0x02,
            AdcPGAGain::X8 => 0x03,
        }
    }
}

/// The device has an 8-bit wide configuration register to
/// select for: input channel, conversion mode, conversion
/// rate, and PGA gain. This register allows the user to
/// change the operating condition of the device and check
/// the status of the device operation.
#[derive(Debug, Copy, Clone)]
pub struct AdcMeterConfig {
    /// RDY: Ready Bit
    ///
    /// This bit is the data ready flag. In read mode, this bit indicates if the output register has been updated
    /// with a latest conversion result. In One-Shot Conversion mode, writing this bit to “1” initiates a new
    /// conversion.
    ///
    /// Reading RDY bit with the read command:
    /// - 1 = Output register has not been updated
    /// - 0 = Output register has been updated with the latest conversion result
    ///
    /// Writing RDY bit with the write command:
    /// Continuous Conversion mode: No effect
    ///
    /// One-Shot Conversion mode:
    /// - 1 = Initiate a new conversion
    /// - 0 = No effect
    pub rdy: bool,
    /// - 00 = Select Channel 1 (Default)
    /// - 01 = Select Channel 2
    /// - 10 = Select Channel 3
    /// - 11 = Select Channel 4 (not connected on OVPinergy)
    pub channel: AdcChannel,
    /// - 1 = Continuous Conversion Mode (Default). The device performs data conversions continuously
    /// - 0 = One-Shot Conversion Mode. The device performs a single conversion and enters a low power
    /// standby mode until it receives another write or read command
    pub conv_mode: bool,
    /// - 00 = 240 SPS (12 bits) (Default)
    /// - 01 = 60 SPS (14 bits)
    /// - 10 = 15 SPS (16 bits)
    /// - 11 = 3.75 SPS (18 bits)
    pub sample_rate: AdcSampleRate,
    /// - 00 = x1 (Default)
    /// - 01 = x2
    /// - 10 = x4
    /// - 11 = x8
    pub gain: AdcPGAGain,
}

impl Default for AdcMeterConfig {
    fn default() -> Self {
        AdcMeterConfig {
            rdy: true,
            channel: Default::default(),
            conv_mode: true,
            sample_rate: Default::default(),
            gain: Default::default(),
        }
    }
}

impl fmt::Display for AdcMeterConfig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Ready      : {}", self.rdy)?;
        writeln!(f, "Channel    : {}", self.channel)?;
        writeln!(f, "Conv Mode  : {}", self.conv_mode)?;
        writeln!(f, "Sample Rate: {}", self.sample_rate)?;
        writeln!(f, "Gain       : {}", self.gain)
    }
}

impl TryFrom<u8> for AdcMeterConfig {
    type Error = AdcError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(AdcMeterConfig {
            rdy: value & 0x80 != 0,
            channel: AdcChannel::try_from(value)?,
            conv_mode: 0x10 != 0,
            sample_rate: AdcSampleRate::from(value),
            gain: AdcPGAGain::from(value),
        })
    }
}

impl From<&AdcMeterConfig> for u8 {
    fn from(config: &AdcMeterConfig) -> Self {
        let mut value: u8 = if config.rdy { 0x80 } else { 0x00 };
        value |= u8::from(&config.channel);
        value |= if config.conv_mode { 0x10 } else { 0x00 };
        value |= u8::from(config.sample_rate);
        value |= u8::from(config.gain);
        value
    }
}

/// Structure to handle the I2C ADC meter current communication
///
/// ```
/// use rs_ovpinergy::adcmeter::{AdcMeter, AdcMeterConfig, AdcSampleRate};
///
/// fn read_val() {
///     // Create a new meter object and reset the MCP3424 to avoid any problem during bootup
///     let mut meter = AdcMeter::new().unwrap();
///     meter.reset().unwrap();
///
///     // Create a meter config with default value
///     let meter_config = AdcMeterConfig::default();
///
///     // Write the configuration registry
///     meter.write_config(&meter_config).unwrap();
///
///     if let Ok((val, config)) = meter.get_raw(&meter_config.sample_rate) {
///         assert_eq!(meter_config.channel, config.channel);
///         println!("Meter value: {} for {:?}", val, config);
///     }
/// }
/// ```
#[derive(Debug)]
pub struct AdcMeter {
    adc_addr: u16,
    bus: I2c,
}

impl AdcMeter {
    /// Construct a new AdcMeter to read ADC inputs (with default address 0x68)
    pub fn new() -> i2c::Result<AdcMeter> {
        let adc_addr = 0x68;
        let mut bus = I2c::new()?;
        bus.set_slave_address(adc_addr)?;
        Ok(AdcMeter { bus, adc_addr })
    }

    /// Construct a new AdcMeter to read ADC inputs with a custom slave address (instead of 0x68)
    pub fn with_addr(adc_addr: u16) -> i2c::Result<AdcMeter> {
        let mut bus = I2c::new()?;
        bus.set_slave_address(adc_addr)?;
        Ok(AdcMeter { bus, adc_addr })
    }

    /// Method to reset the ADC converter
    ///
    /// At the acknowledgement of this
    /// byte, the device will abort current conversion and
    /// perform the following tasks:
    /// (a) Internal reset similar to a Power-On-Reset (POR).
    /// All configuration and data register bits are reset to
    /// default values.
    /// (b) Latch the logic status of external address selection
    /// pins (Adr0 and Adr1 pins)
    pub fn reset(&mut self) -> Result<(), AdcError> {
        self.bus.smbus_send_byte(0x06)?;
        Ok(())
    }

    /// Method to latch the ADC converter
    ///
    /// The device will latch the logic
    /// status of the external address selection pins (Adr0 and
    /// Adr1 pins), but will not perform a reset.
    pub fn latch(&mut self) -> Result<(), AdcError> {
        self.bus.smbus_send_byte(0x04)?;
        Ok(())
    }

    /// Method to write a config to the ADC meter component
    pub fn write_config(&self, config: &AdcMeterConfig) -> Result<(), AdcError> {
        self.bus.smbus_send_byte(config.into())?;
        Ok(())
    }

    /// Getter of the raw value read from the ADC
    /// Return also the configuration from the read
    pub fn get_raw(
        &mut self,
        sample_rate: &AdcSampleRate,
    ) -> Result<(u32, AdcMeterConfig), AdcError> {
        let output_size = sample_rate.get_value_output_size();
        let mut buffer = vec![0; output_size];
        self.bus.read(&mut buffer)?;
        let config =
            AdcMeterConfig::try_from(buffer.pop().ok_or(AdcError::WrongConfiguration(0xFF))?)?;
        if output_size == 4 {
            if buffer[0] != 0xFF || buffer[1] != 0xFF || buffer[2] != 0xFF {
                Ok((
                    u32::from_be_bytes([0, buffer[0], buffer[1], buffer[2]]),
                    config,
                ))
            } else {
                Err(AdcError::MissingValue(config))
            }
        } else if buffer[0] != 0xFF || buffer[1] != 0xFF {
            Ok((u16::from_be_bytes([buffer[0], buffer[1]]) as u32, config))
        } else {
            Err(AdcError::MissingValue(config))
        }
    }

    /// Method to get a current value (in mA) out of a raw ADC value
    pub fn convert_raw_to_current(val: u32, config: &AdcMeterConfig) -> f64 {
        let mut ret_val = (val * 1000) as f64;
        ret_val /= config.gain.get_gain();
        ret_val /= config.sample_rate.get_current_coef();
        ret_val
    }

    /// Method to get a task for ADC polling
    ///
    /// ```
    /// use tokio::sync::mpsc;
    /// use tokio::time::Duration;
    /// use rs_ovpinergy::CardEvent;
    /// use rs_ovpinergy::adcmeter::{AdcMeter, AdcMeterConfig, AdcChannel, AdcSampleRate};
    ///
    /// async fn read_val() {
    ///     // Create a new meter object and reset the MCP3424 to avoid any problem during bootup
    ///     let mut meter = AdcMeter::new().unwrap();
    ///     meter.reset().unwrap();
    ///
    ///     // Create a meter channel and sample rate
    ///     let meter_channel = AdcChannel::default();
    ///     let meter_sample_rate = AdcSampleRate::default();
    ///
    ///     let (tx, mut rx) = mpsc::channel(100);
    ///     let _ = meter.into_task(tx.clone(), meter_channel, meter_sample_rate, Duration::from_secs(60));
    ///     if let Some(CardEvent::AdcMeasure(Ok(val))) = rx.recv().await {
    ///         println!("Meter value: {}", val);
    ///     }
    /// }
    /// ```
    pub async fn into_task(
        mut self,
        event_queue: mpsc::Sender<CardEvent>,
        channel: AdcChannel,
        sample_rate: AdcSampleRate,
        interval_measure: Duration,
    ) -> Result<JoinHandle<()>, AdcError> {
        let meter_config = AdcMeterConfig {
            channel,
            sample_rate,
            ..Default::default()
        };
        self.write_config(&meter_config)?;

        Ok(tokio::spawn(async move {
            let mut interval_measures = interval(Duration::from_millis(
                meter_config.sample_rate.get_period_ms(),
            ));
            let mut interval_end = interval_at(Instant::now() + interval_measure, interval_measure);
            let mut values = Vec::new();
            loop {
                tokio::select! {
                    _ = interval_measures.tick() => {
                        if let Ok((val, _)) = self.get_raw(&meter_config.sample_rate) {
                            values.push(val as u64);
                        }
                    }
                    _ = interval_end.tick() => {
                        let _ = if !values.is_empty() {
                            let value = Self::convert_raw_to_current(
                                (values.iter().sum::<u64>() / values.len() as u64) as u32,
                                &meter_config,
                            );
                            values.clear();
                            event_queue.send(CardEvent::AdcMeasure(Ok(value))).await
                        } else {
                            event_queue.send(CardEvent::AdcMeasure(Err(AdcError::MissingValue(meter_config)))).await
                        };
                    }
                }
            }
        }))
    }
}

impl Clone for AdcMeter {
    fn clone(&self) -> Self {
        Self::with_addr(self.adc_addr).unwrap()
    }
}
