use std::{thread::sleep, time::Duration};

use rs_ovpinergy::adcmeter::{AdcMeter, AdcMeterConfig, AdcSampleRate};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut meter = AdcMeter::new()?;
    meter.reset()?;
    println!("Init the current meter {:?}", meter);

    // Set the sample rate to the maximum to have the best precision
    let sample_rate = AdcSampleRate::Rate3_7;
    let meter_config = AdcMeterConfig {
        sample_rate,
        ..Default::default()
    };

    println!("Configuration:\n{}", meter_config);

    // Send the configuration
    meter.write_config(&meter_config)?;
    loop {
        if let Ok((val, config)) = meter.get_raw(&sample_rate) {
            println!("Meter value: {} for {}", val, config);
        }

        sleep(Duration::from_millis(sample_rate.get_period_ms()));
    }
}
