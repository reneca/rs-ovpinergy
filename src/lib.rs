//! Rust library to use an [OVPinergy](https://www.overware.fr/ovpinergy) card.
//! This card is an expansion of a Raspberry Pi to handle energy in a house.

#![warn(missing_docs)]

use adcmeter::AdcError;
use teleinfo::{RateColor, RatePeriod, TeleinfoData};
pub mod adcmeter;
pub mod homeio;
pub mod teleinfo;

/// Enum that list all OVPinergy card event to process them asynchronously
#[derive(Debug, Default)]
pub enum CardEvent {
    /// No event
    #[default]
    None,
    /// Mesure of an ADC channel
    AdcMeasure(Result<f64, AdcError>),
    /// Mesure of a teleinfo data
    TeleinfoMeasure(TeleinfoData),
    /// Price rate period is changing
    PriceRatePeriodChange(RatePeriod),
    /// Color change advice (for EJP or Tempo)
    ColorAdvice(RateColor),
}
