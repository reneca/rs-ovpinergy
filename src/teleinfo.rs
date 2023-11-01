//! Teleinfo module to handle the counter metrics
use std::{
    fmt,
    io::{self, ErrorKind},
    path::Path,
    time::Duration,
};

use bytes::{Buf, BytesMut};
use rppal::uart::{self, Parity, Uart};
use rs_ovpinergy_macros::teleinfo_checksum_str;
use tokio::{
    sync::{mpsc, watch},
    task::JoinHandle,
    time::{interval_at, Instant},
};
use tracing::warn;

use crate::CardEvent;

/// Rate period useful to know the current price
#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub enum RatePeriod {
    /// Every hours
    #[default]
    TH,
    /// Dig hours
    HC,
    /// Full hours
    HP,
    /// Standard hours
    HN,
    /// Mobile spike hours
    PM,
}

impl TryFrom<String> for RatePeriod {
    type Error = uart::Error;

    fn try_from(val: String) -> Result<RatePeriod, Self::Error> {
        if val.starts_with("TH") {
            Ok(RatePeriod::TH)
        } else if val.starts_with("HC") {
            Ok(RatePeriod::HC)
        } else if val.starts_with("HP") {
            Ok(RatePeriod::HP)
        } else if val.starts_with("HN") {
            Ok(RatePeriod::HN)
        } else if val.starts_with("PM") {
            Ok(RatePeriod::PM)
        } else {
            Err(uart::Error::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("Wrong rate period {}", val),
            )))
        }
    }
}

impl fmt::Display for RatePeriod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RatePeriod::TH => write!(f, "Every hours"),
            RatePeriod::HC => write!(f, "Dig hours"),
            RatePeriod::HP => write!(f, "Full hours"),
            RatePeriod::HN => write!(f, "Standard hours"),
            RatePeriod::PM => write!(f, "Mibile spike hours"),
        }
    }
}

/// Rate color useful to know the price
#[derive(Default, Debug, Copy, Clone, PartialEq)]
#[repr(usize)]
pub enum RateColor {
    /// Unknown color (or not applicable)
    #[default]
    UNKNOWN = 0,
    /// Blue price (lower)
    BLUE = 1,
    /// White price (middle price)
    WHITE = 3,
    /// Red price (most expansive)
    RED = 2,
}

impl From<String> for RateColor {
    fn from(val: String) -> Self {
        let upper_val = val.to_uppercase();
        if upper_val.starts_with("BLEU") || upper_val.starts_with("BLU") {
            RateColor::BLUE
        } else if upper_val.starts_with("BLAN") || upper_val.starts_with("WHIT") {
            RateColor::WHITE
        } else if upper_val.starts_with("ROUG") || upper_val.starts_with("RED") {
            RateColor::RED
        } else {
            RateColor::UNKNOWN
        }
    }
}

impl fmt::Display for RateColor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RateColor::UNKNOWN => write!(f, "Unknown"),
            RateColor::BLUE => write!(f, "Blue"),
            RateColor::WHITE => write!(f, "White"),
            RateColor::RED => write!(f, "Red"),
        }
    }
}

/// List of Tarif options
#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub enum OpTarif {
    /// Base option
    #[default]
    BASE,
    /// Dig/Full hour option
    HC,
    /// EJP option
    EJP,
    /// Tempo option
    BBRx(u8),
}

impl TryFrom<String> for OpTarif {
    type Error = uart::Error;

    fn try_from(val: String) -> Result<Self, Self::Error> {
        if val.len() == 4 {
            match val.as_str() {
                "BASE" => Ok(OpTarif::BASE),
                "HC.." => Ok(OpTarif::HC),
                "EJP." => Ok(OpTarif::EJP),
                bbr if bbr.starts_with("BBR") => {
                    Ok(OpTarif::BBRx(bbr.chars().last().unwrap() as u8))
                }
                _ => Err(uart::Error::Io(io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Wrong optarif value {}", val),
                ))),
            }
        } else {
            Err(uart::Error::Io(io::Error::new(
                ErrorKind::InvalidInput,
                format!("Wrong optarif length {}", val),
            )))
        }
    }
}

impl OpTarif {
    fn get_checksum(&self) -> u64 {
        match self {
            OpTarif::BASE => 283,
            OpTarif::HC => 231,
            OpTarif::EJP => 269,
            OpTarif::BBRx(id) => 214 + *id as u64,
        }
    }
}

/// Teleinfo line data
#[derive(Debug, Clone, PartialEq)]
pub enum TeleinfoFrame {
    /// Address of the meter
    ADCO(String),
    /// Secondary address of the meter
    ADSC(String),
    /// Tarif option
    OPTARIF(OpTarif),
    /// Subscribe amps
    ISOUSC(u8),
    /// Base index option (Wh)
    BASE(u32),
    /// Dig index option (Wh)
    HCHC(u32),
    /// Full index option (Wh)
    HCHP(u32),
    /// EJP Normal index (Wh)
    EJPHN(u32),
    /// EJP Mobile peak index (Wh)
    EJPHPM(u32),
    /// Blue day dig index (Wh)
    BBRHCJB(u32),
    /// Blue day full index (Wh)
    BBRHPJB(u32),
    /// White day dig index (Wh)
    BBRHCJW(u32),
    /// White day full index (Wh)
    BBRHPJW(u32),
    /// Red day dig index (Wh)
    BBRHCJR(u32),
    /// Red day full index (Wh)
    BBRHPJR(u32),
    /// EJP start notice (30 minutes)
    PEJP(u8),
    /// Current period pricing
    PTEC(String),
    /// Color of tomorrow pricing day
    DEMAIN(String),
    /// Instantaneous current per phase
    IINST(u8, u16),
    /// Maximum current per phase
    IMAX(u8, u16),
    /// Warning on current overload per phase
    ADIR(u8, u16),
    /// Max power reach (W)
    PMAX(u32),
    /// Appear power (VA)
    PAPP(u32),
    /// Low/High period
    HHPHC(char),
    /// State word of the meter
    MOTDETAT(String),
    /// Potential presence
    PPOT(u8),
}

macro_rules! checksum_str {
    ($checksum:ident,$name:expr,$s:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        for c in $s.chars() {
            $checksum += c as u64;
        }
    }};
}

macro_rules! checksum_num {
    ($checksum:ident,$name:expr,$num:ident) => {{
        $checksum += teleinfo_checksum_str!($name);
        let val = *$num as u64;
        $checksum += val % 10 + 48;
        $checksum += val / 10 + 48;
    }};
    ($checksum:ident,$name:expr,$num:ident,$n:expr) => {{
        $checksum += teleinfo_checksum_str!($name);
        $checksum += TeleinfoFrame::checksum_num($num, $n);
    }};
    ($checksum:ident,$name:expr,$name_id:expr,$num:ident,$n:expr) => {{
        $checksum += teleinfo_checksum_str!($name);
        if $name_id > 0 {
            $checksum += $name_id as u64 + 48
        }
        $checksum += TeleinfoFrame::checksum_num($num, $n);
    }};
}

impl TeleinfoFrame {
    fn get_phase_id(tag: &str) -> u8 {
        if let Some(c) = tag.chars().last() {
            let last_char = c as u8;
            if (48..=51).contains(&last_char) {
                return last_char - 48;
            }
        }

        0
    }

    fn read_val(buf: &mut dyn Buf) -> uart::Result<(u8, String)> {
        let mut val = String::with_capacity(12);
        let sep_char = loop {
            if buf.has_remaining() {
                let char = buf.get_u8();
                if char == 0x09 || char == 0x20 {
                    break char;
                } else if char > 0x20 && char < 0x7E {
                    val.push(char as char);
                } else {
                    return Err(uart::Error::Io(io::Error::new(
                        ErrorKind::InvalidInput,
                        format!("Wrong value identifier char {}, parsed {}", char, val),
                    )));
                }
            } else {
                return Err(uart::Error::Io(io::Error::new(
                    ErrorKind::WouldBlock,
                    format!("Not enough data to read val {}..", val),
                )));
            }
        };

        Ok((sep_char, val))
    }

    fn get_val(buf: &mut dyn Buf, expected_sep: u8) -> uart::Result<String> {
        let (val_sep, val) = TeleinfoFrame::read_val(buf)?;
        if expected_sep != val_sep {
            return Err(uart::Error::Io(io::Error::new(
                ErrorKind::InvalidData,
                format!("Unexpected separator encounter {} for ADCO", val_sep),
            )));
        }

        Ok(val)
    }

    fn get_num_val<T>(buf: &mut dyn Buf, expected_sep: u8) -> uart::Result<T>
    where
        T: std::str::FromStr,
        <T as std::str::FromStr>::Err: std::fmt::Display,
    {
        TeleinfoFrame::get_val(buf, expected_sep)?
            .parse::<T>()
            .map_err(|e| -> uart::Error {
                uart::Error::Io(io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Can't parse numerical value {}", e),
                ))
            })
    }

    fn checksum_num<T>(val: T, pad: u8) -> u64
    where
        T: std::string::ToString,
    {
        let val_str = val.to_string();
        let mut checksum = if val_str.len() < pad as usize {
            (pad as u64 - val_str.len() as u64) * 48
        } else {
            0
        };

        for c in val_str.chars() {
            checksum += c as u64;
        }

        checksum
    }

    fn checksum(&self, sep: u8) -> u8 {
        let mut checksum = sep as u64;
        match self {
            TeleinfoFrame::ADCO(val) => checksum_str!(checksum, "ADCO", val),
            TeleinfoFrame::ADSC(val) => checksum_str!(checksum, "ADSC", val),
            TeleinfoFrame::OPTARIF(tarif) => {
                checksum += teleinfo_checksum_str!("OPTARIF");
                checksum += tarif.get_checksum();
            }
            TeleinfoFrame::ISOUSC(amps) => checksum_num!(checksum, "ISOUSC", amps),
            TeleinfoFrame::BASE(wh) => checksum_num!(checksum, "BASE", wh, 9),
            TeleinfoFrame::HCHC(wh) => checksum_num!(checksum, "HCHC", wh, 9),
            TeleinfoFrame::HCHP(wh) => checksum_num!(checksum, "HCHP", wh, 9),
            TeleinfoFrame::EJPHN(wh) => checksum_num!(checksum, "EJPHN", wh, 9),
            TeleinfoFrame::EJPHPM(wh) => checksum_num!(checksum, "EJPHPM", wh, 9),
            TeleinfoFrame::BBRHCJB(wh) => checksum_num!(checksum, "BBRHCJB", wh, 9),
            TeleinfoFrame::BBRHPJB(wh) => checksum_num!(checksum, "BBRHPJB", wh, 9),
            TeleinfoFrame::BBRHCJW(wh) => checksum_num!(checksum, "BBRHCJW", wh, 9),
            TeleinfoFrame::BBRHPJW(wh) => checksum_num!(checksum, "BBRHPJW", wh, 9),
            TeleinfoFrame::BBRHCJR(wh) => checksum_num!(checksum, "BBRHCJR", wh, 9),
            TeleinfoFrame::BBRHPJR(wh) => checksum_num!(checksum, "BBRHPJR", wh, 9),
            TeleinfoFrame::PEJP(advice) => checksum_num!(checksum, "PEJP", advice),
            TeleinfoFrame::PTEC(val) => checksum_str!(checksum, "PTEC", val),
            TeleinfoFrame::DEMAIN(val) => checksum_str!(checksum, "DEMAIN", val),
            TeleinfoFrame::IINST(phase, amps) => checksum_num!(checksum, "IINST", *phase, amps, 3),
            TeleinfoFrame::IMAX(phase, amps) => checksum_num!(checksum, "IMAX", *phase, amps, 3),
            TeleinfoFrame::ADIR(phase, amps) => checksum_num!(checksum, "ADIR", *phase, amps, 3),
            TeleinfoFrame::PMAX(w) => checksum_num!(checksum, "PMAX", w, 5),
            TeleinfoFrame::PAPP(va) => checksum_num!(checksum, "PAPP", va, 5),
            TeleinfoFrame::HHPHC(hphc) => checksum_num!(checksum, "HHPHC", hphc, 1),
            TeleinfoFrame::MOTDETAT(val) => checksum_str!(checksum, "MOTDETAT", val),
            TeleinfoFrame::PPOT(ppot) => checksum_num!(checksum, "PPOT", ppot, 2),
        }

        (checksum & 0x3F) as u8 + sep
    }

    /// Method to read a Teleinfo frame
    pub fn read(buf: &mut dyn Buf) -> uart::Result<TeleinfoFrame> {
        let (tag_sep, tag) = TeleinfoFrame::read_val(buf)?;
        let teleinfo_data = match tag.as_str() {
            "ADCO" => TeleinfoFrame::ADCO(TeleinfoFrame::get_val(buf, tag_sep)?),
            "ADSC" => TeleinfoFrame::ADCO(TeleinfoFrame::get_val(buf, tag_sep)?),
            "OPTARIF" => {
                TeleinfoFrame::OPTARIF(OpTarif::try_from(TeleinfoFrame::get_val(buf, tag_sep)?)?)
            }
            "ISOUSC" => TeleinfoFrame::ISOUSC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BASE" => TeleinfoFrame::BASE(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "HCHC" => TeleinfoFrame::HCHC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "HCHP" => TeleinfoFrame::HCHP(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EJPHN" => TeleinfoFrame::EJPHN(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "EJPHPM" => TeleinfoFrame::EJPHPM(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJB" => TeleinfoFrame::BBRHCJB(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJB" => TeleinfoFrame::BBRHPJB(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJW" => TeleinfoFrame::BBRHCJW(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJW" => TeleinfoFrame::BBRHPJW(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHCJR" => TeleinfoFrame::BBRHCJR(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "BBRHPJR" => TeleinfoFrame::BBRHPJR(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PEJP" => TeleinfoFrame::PEJP(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PTEC" => TeleinfoFrame::PTEC(TeleinfoFrame::get_val(buf, tag_sep)?),
            "DEMAIN" => TeleinfoFrame::DEMAIN(TeleinfoFrame::get_val(buf, tag_sep)?),
            iinst if iinst.starts_with("IINST") => TeleinfoFrame::IINST(
                Self::get_phase_id(iinst),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            imax if imax.starts_with("IMAX") => TeleinfoFrame::IMAX(
                Self::get_phase_id(imax),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            adir if adir.starts_with("ADIR") => TeleinfoFrame::ADIR(
                Self::get_phase_id(adir),
                TeleinfoFrame::get_num_val(buf, tag_sep)?,
            ),
            "PMAX" => TeleinfoFrame::PMAX(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "PAPP" => TeleinfoFrame::PAPP(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "HHPHC" => TeleinfoFrame::HHPHC(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            "MOTDETAT" => TeleinfoFrame::MOTDETAT(TeleinfoFrame::get_val(buf, tag_sep)?),
            "PPOT" => TeleinfoFrame::PPOT(TeleinfoFrame::get_num_val(buf, tag_sep)?),
            tag => {
                return Err(uart::Error::Io(io::Error::new(
                    ErrorKind::InvalidInput,
                    format!("Unhandle teleinfo tag {}", tag),
                )))
            }
        };

        if buf.remaining() >= 2 {
            let checksum = buf.get_u8();
            let line_end = buf.get_u8();
            if line_end == 0x0D {
                let teleinfo_checksum = teleinfo_data.checksum(tag_sep);
                if checksum == teleinfo_checksum {
                    Ok(teleinfo_data)
                } else {
                    Err(uart::Error::Io(io::Error::new(
                        ErrorKind::InvalidData,
                        format!(
                            "Wrong checksum {}, expected {} for {}",
                            checksum, teleinfo_checksum, tag
                        ),
                    )))
                }
            } else {
                Err(uart::Error::Io(io::Error::new(
                    ErrorKind::UnexpectedEof,
                    format!("Unexpected teleinfo EOF {}", line_end),
                )))
            }
        } else {
            Err(uart::Error::Io(io::Error::new(
                ErrorKind::WouldBlock,
                "Not enough data to read the checksum",
            )))
        }
    }
}

/// Teleinfo Data aggregation
#[derive(Default, Debug, Clone)]
pub struct TeleinfoData {
    meter_address: String,
    tarif_option: OpTarif,
    subscribe_amps: u8,

    base_counter: u64,
    low_hour: [u32; 4],
    high_hour: [u32; 4],
    period_rate: RatePeriod,
    tomorrow_color: RateColor,

    instantaneous_current: [u16; 4],
    max_current: [u16; 4],
    warn_current: [u16; 4],

    appearing_power: u32,
    max_power: u32,

    low_high_period: char,
    meter_status: String,
}

impl TeleinfoData {
    fn new(address: String) -> TeleinfoData {
        TeleinfoData {
            meter_address: address,
            ..Default::default()
        }
    }

    fn has_address(&self) -> bool {
        !self.meter_address.is_empty()
    }

    fn update_base_counter(&mut self) {
        self.base_counter = 0;
        for i in 0..4 {
            self.base_counter += self.low_hour[i] as u64;
        }
        for i in 0..4 {
            self.base_counter += self.high_hour[i] as u64;
        }
    }

    /// Method to update the Teleinfo from incomming Teleinfo frame
    fn update(&mut self, frame: TeleinfoFrame) -> uart::Result<()> {
        match frame {
            TeleinfoFrame::ADCO(address) => self.meter_address = address,
            TeleinfoFrame::ADSC(second_addr) => {
                self.meter_address += &("/".to_owned() + second_addr.as_str())
            }
            TeleinfoFrame::OPTARIF(tarif) => self.tarif_option = tarif,
            TeleinfoFrame::ISOUSC(amps) => self.subscribe_amps = amps,
            TeleinfoFrame::BASE(wh) => self.base_counter = wh as u64,
            TeleinfoFrame::HCHC(wh) => self.low_hour[RateColor::UNKNOWN as usize] = wh,
            TeleinfoFrame::HCHP(wh) => self.high_hour[RateColor::UNKNOWN as usize] = wh,
            TeleinfoFrame::EJPHN(wh) => self.low_hour[RateColor::BLUE as usize] = wh,
            TeleinfoFrame::EJPHPM(wh) => self.high_hour[RateColor::RED as usize] = wh,
            TeleinfoFrame::BBRHCJB(wh) => self.low_hour[RateColor::BLUE as usize] = wh,
            TeleinfoFrame::BBRHPJB(wh) => self.high_hour[RateColor::BLUE as usize] = wh,
            TeleinfoFrame::BBRHCJW(wh) => self.low_hour[RateColor::WHITE as usize] = wh,
            TeleinfoFrame::BBRHPJW(wh) => self.high_hour[RateColor::WHITE as usize] = wh,
            TeleinfoFrame::BBRHCJR(wh) => self.low_hour[RateColor::RED as usize] = wh,
            TeleinfoFrame::BBRHPJR(wh) => self.high_hour[RateColor::RED as usize] = wh,
            TeleinfoFrame::PEJP(advice) => {
                if advice == 30 {
                    self.tomorrow_color = RateColor::RED;
                } else {
                    self.tomorrow_color = RateColor::BLUE;
                }
            }
            TeleinfoFrame::PTEC(val) => self.tarif_option = OpTarif::try_from(val)?,
            TeleinfoFrame::DEMAIN(val) => self.tomorrow_color = RateColor::from(val),
            TeleinfoFrame::IINST(phase, amps) => self.instantaneous_current[phase as usize] = amps,
            TeleinfoFrame::IMAX(phase, amps) => self.max_current[phase as usize] = amps,
            TeleinfoFrame::ADIR(phase, amps) => self.warn_current[phase as usize] = amps,
            TeleinfoFrame::PMAX(w) => self.appearing_power = w,
            TeleinfoFrame::PAPP(va) => self.max_power = va,
            TeleinfoFrame::HHPHC(period) => self.low_high_period = period,
            TeleinfoFrame::MOTDETAT(val) => self.meter_status = val,
            TeleinfoFrame::PPOT(_) => {}
        }

        Ok(())
    }
}

impl fmt::Display for TeleinfoData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "Meter[{}] {:?}, Period {}, PeriodHour {} - {}",
            self.meter_address,
            self.tarif_option,
            self.period_rate,
            self.low_high_period,
            self.meter_status
        )?;

        writeln!(
            f,
            "Amps {}A / {}A",
            self.instantaneous_current[0], self.subscribe_amps
        )?;
        if self.max_current[1] > 0 {
            for i in 1..3 {
                writeln!(
                    f,
                    "  - Phase {} {}A / {}A",
                    i, self.instantaneous_current[i], self.max_current[i]
                )?;
            }
        }

        writeln!(f, "Power {}W / {} VA", self.appearing_power, self.max_power)?;

        if self.tomorrow_color != RateColor::UNKNOWN {
            writeln!(f, "Tomorrow color {}", self.tomorrow_color)?;
        }

        Ok(())
    }
}

/// Structure to handle the teleinfo communication
#[derive(Debug)]
pub struct Teleinfo {
    serial: Uart,
    read_data: BytesMut,
}

impl Teleinfo {
    /// Function to create a new Teleinfo, it connect to the serial of the [OVPinergy Teleinfo](https://www.overware.fr/ovpinergy/#lecture-t%C3%A9l%C3%A9info)
    pub fn new(legacy: bool) -> uart::Result<Teleinfo> {
        Self::with_path("/dev/serial0", legacy)
    }

    /// Function to create a new Teleinfo, it connect to the specified path serial
    pub fn with_path<P: AsRef<Path>>(path: P, legacy: bool) -> uart::Result<Teleinfo> {
        let mut serial = if legacy {
            Uart::with_path(path, 1200, Parity::Even, 7, 1)
        } else {
            Uart::with_path(path, 9600, Parity::Even, 7, 1)
        }?;

        serial.set_read_mode(25, Duration::default())?;
        Ok(Teleinfo {
            serial,
            read_data: BytesMut::with_capacity(128),
        })
    }

    /// Method to read the teleinfo in blocking mode
    pub fn block_read(&mut self) -> uart::Result<TeleinfoFrame> {
        loop {
            if !self.read_data.len() > 10 {
                let mut data = self.read_data.chunk();
                match TeleinfoFrame::read(&mut data) {
                    Ok(teleinfo_data) => {
                        let sep = if self.read_data.is_empty() {
                            0x0A
                        } else {
                            self.read_data.get_u8()
                        };

                        if sep == 0x0A {
                            return Ok(teleinfo_data);
                        } else {
                            return Err(uart::Error::Io(io::Error::new(
                                ErrorKind::InvalidInput,
                                format!("Don't have valid new line beginning: {}", sep),
                            )));
                        }
                    }
                    Err(uart::Error::Io(io_err)) => {
                        if io_err.kind() == ErrorKind::WouldBlock {
                            let _ = self.serial.read(self.read_data.as_mut())?;
                            continue;
                        } else {
                            return Err(uart::Error::Io(io_err));
                        }
                    }
                    Err(e) => return Err(e),
                }
            } else {
                // Read and reach the beginning of a line
                let _ = self.serial.read(self.read_data.as_mut())?;
                while !self.read_data.is_empty() && self.read_data.get_u8() != 0x0A {}
            }
        }
    }

    /// Method to get a task for Teleinfo polling
    pub async fn into_task(
        mut self,
        event_queue: mpsc::Sender<CardEvent>,
        interval_measure: Duration,
    ) -> JoinHandle<()> {
        // Task to loop on Teleinfo data received
        let (tx_teleinfo, mut rx_teleinfo) = watch::channel(TeleinfoData::default());
        tokio::spawn(async move {
            let mut data = TeleinfoData::default();
            loop {
                match self.block_read() {
                    Ok(frame) => {
                        if let TeleinfoFrame::ADCO(address) = frame {
                            let mut last_data = data;
                            data = TeleinfoData::new(address);

                            if last_data.has_address() {
                                if last_data.base_counter == 0 {
                                    last_data.update_base_counter();
                                }
                                if tx_teleinfo.send(last_data).is_err() {
                                    break;
                                }
                            }
                        } else if let Err(e) = data.update(frame) {
                            warn!("Teleinfo data update error: {}", e);
                            data = TeleinfoData::default();
                        }
                    }
                    Err(e) => {
                        warn!("Teleinfo error: {}", e);
                        // TODO handle error by rebooting the connection
                    }
                };
            }
        });

        // Task to send periodic measure and event on status change
        tokio::spawn(async move {
            let mut interval_end = interval_at(Instant::now() + interval_measure, interval_measure);
            let mut period_rate = RatePeriod::default();
            let mut tomorrow_advice = RateColor::default();
            loop {
                tokio::select! {
                    Ok(()) = rx_teleinfo.changed() => {
                        let mut change_period_rate = None;
                        let mut tomorrow_notice = None;
                        {
                            let data = rx_teleinfo.borrow_and_update();
                            if data.period_rate != period_rate {
                                period_rate = data.period_rate;
                                change_period_rate = Some(data.period_rate);
                            }

                            if data.tomorrow_color != tomorrow_advice {
                                tomorrow_advice = data.tomorrow_color;
                                tomorrow_notice = Some(data.tomorrow_color);
                            }
                        }

                        // Notify a period rate change
                        if let Some(period_rate) = change_period_rate {
                            let _ = event_queue.send(CardEvent::PriceRatePeriodChange(period_rate)).await;
                        }

                        // Notify a color advice
                        if let Some(tomorrow_advice) = tomorrow_notice {
                            let _ = event_queue.send(CardEvent::ColorAdvice(tomorrow_advice)).await;
                        }
                    }
                    _ = interval_end.tick() => {
                        let data = rx_teleinfo.borrow().clone();
                        let _ = event_queue.send(CardEvent::TeleinfoMeasure(data)).await;
                    }
                }
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use bytes::Bytes;

    use crate::teleinfo::TeleinfoFrame;

    #[test]
    fn teleinfo_frame() {
        {
            let mut frame_data_legacy = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x20, 0x54, 0x45, 0x53, 0x54, 0x20, 0x57, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy).unwrap();
            assert_eq!(TeleinfoFrame::ADCO(String::from("TEST")), frame);
        }

        {
            let mut frame_data_legacy_wrong_checksum = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x20, 0x54, 0x45, 0x53, 0x54, 0x20, 0x34, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy_wrong_checksum).unwrap_err();
            assert_eq!(
                String::from("I/O error: Wrong checksum 52, expected 87 for ADCO"),
                frame.to_string()
            );
        }

        {
            let mut frame_data_legacy_wrong_tag =
                Bytes::from_static(&[0x4Eu8, 0x4F, 0x4E, 0x45, 0x20]);
            let frame = TeleinfoFrame::read(&mut frame_data_legacy_wrong_tag).unwrap_err();
            assert_eq!(
                String::from("I/O error: Unhandle teleinfo tag NONE"),
                frame.to_string()
            );
        }

        {
            let mut frame_data = Bytes::from_static(&[
                0x41u8, 0x44, 0x43, 0x4F, 0x09, 0x54, 0x45, 0x53, 0x54, 0x09, 0x29, 0x0D,
            ]);
            let frame = TeleinfoFrame::read(&mut frame_data).unwrap();
            assert_eq!(TeleinfoFrame::ADCO(String::from("TEST")), frame);
        }
    }
}
