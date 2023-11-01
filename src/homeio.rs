//! OVPinergy module to handle the GPIO card
use std::{
    io,
    sync::{Arc, Mutex, MutexGuard},
};

use rppal::gpio::{self, Gpio, InputPin, Level, OutputPin};
use tokio::task;

/// Structure to handle an output LED
pub struct OutputLed {
    red: OutputPin,
    green: OutputPin,
    blue: OutputPin,
}

impl OutputLed {
    fn new(gpio: &Gpio, red: u8, green: u8, blue: u8) -> gpio::Result<OutputLed> {
        Ok(OutputLed {
            red: gpio.get(red)?.into_output(),
            green: gpio.get(green)?.into_output(),
            blue: gpio.get(blue)?.into_output(),
        })
    }

    /// Setter of the LED color
    pub fn set(&mut self, red: bool, green: bool, blue: bool) {
        if red {
            self.red.set_high();
        } else {
            self.red.set_low();
        }

        if green {
            self.green.set_high();
        } else {
            self.green.set_low();
        }

        if blue {
            self.blue.set_high();
        } else {
            self.blue.set_low();
        }
    }

    /// Method to turn off the LED
    pub fn reset(&mut self) {
        self.red.set_low();
        self.green.set_low();
        self.blue.set_low();
    }
}

/// Enum to define all [pilote wire](https://www.overware.fr/ovpinergy/#commande-fil-pilote) mode
#[derive(Default, Debug, PartialEq)]
pub enum PiloteMode {
    /// Normal mode the heater is on
    Confort = 0,
    /// The heater is in frost free mode
    FrostFree = 1,
    /// The heater is off
    #[default]
    Stop = 2,
    /// The heater is in eco mode
    Eco = 3,
}

impl From<(bool, bool)> for PiloteMode {
    fn from(pos_neg: (bool, bool)) -> Self {
        if pos_neg.0 {
            if pos_neg.1 {
                PiloteMode::Eco
            } else {
                PiloteMode::Stop
            }
        } else if pos_neg.1 {
            PiloteMode::FrostFree
        } else {
            PiloteMode::Confort
        }
    }
}

/// Structure to handle an output pilote wire (use to handle heater)
pub struct OutputPilote {
    pos: OutputPin,
    neg: OutputPin,
}

impl OutputPilote {
    fn new(gpio: &Gpio, pos: u8, neg: u8) -> gpio::Result<OutputPilote> {
        Ok(OutputPilote {
            pos: gpio.get(pos)?.into_output(),
            neg: gpio.get(neg)?.into_output(),
        })
    }

    /// Method to set the pilote wire mode
    pub fn set(&mut self, mode: PiloteMode) {
        match mode {
            PiloteMode::Confort => {
                self.pos.set_low();
                self.neg.set_low();
            }
            PiloteMode::FrostFree => {
                self.pos.set_low();
                self.neg.set_high();
            }
            PiloteMode::Stop => {
                self.pos.set_high();
                self.neg.set_low();
            }
            PiloteMode::Eco => {
                self.pos.set_high();
                self.neg.set_high();
            }
        }
    }
}

/// Structure to handle an output pilote wire (use to handle heater)
pub struct InputPilote {
    pos: Arc<Mutex<InputPin>>,
    neg: Arc<Mutex<InputPin>>,
}

impl InputPilote {
    fn new(gpio: &Gpio, pos: u8, neg: u8) -> gpio::Result<InputPilote> {
        Ok(InputPilote {
            pos: Arc::new(Mutex::new(gpio.get(pos)?.into_input())),
            neg: Arc::new(Mutex::new(gpio.get(neg)?.into_input())),
        })
    }

    /// Method to get the pilote wire mode
    pub fn get(&self) -> PiloteMode {
        let pos_lock: MutexGuard<InputPin> = self.pos.lock().unwrap();
        let neg_lock: MutexGuard<InputPin> = self.neg.lock().unwrap();
        (pos_lock.is_high(), neg_lock.is_high()).into()
    }

    /// Method to init the interrupt detection (everything starting this point will be cached)
    pub fn init_interrupt(&mut self) -> gpio::Result<()> {
        let mut pos_lock: MutexGuard<InputPin> = self.pos.lock().unwrap();
        pos_lock.set_interrupt(gpio::Trigger::Both)?;
        let mut neg_lock: MutexGuard<InputPin> = self.neg.lock().unwrap();
        neg_lock.set_interrupt(gpio::Trigger::Both)
    }

    /// Method to wait a state change on the pilote wire
    /// Setting reset to `false` returns any cached interrupt trigger events if available (since call to `init_interrupt()`). Setting reset to `true` clears all cached events before polling for new events.
    pub async fn interrupt(&mut self, reset: bool) -> gpio::Result<PiloteMode> {
        let mut value = {
            let pos_lock: MutexGuard<InputPin> = self.pos.lock().unwrap();
            let neg_lock: MutexGuard<InputPin> = self.neg.lock().unwrap();
            (pos_lock.is_high(), neg_lock.is_high())
        };

        let pos = self.pos.clone();
        let neg = self.neg.clone();
        let result = tokio::select! {
            pos_handle = task::spawn(async move {
                let mut pos_lock: MutexGuard<InputPin> = pos.lock().unwrap();
                if let Some(level) = pos_lock.poll_interrupt(reset, None)? {
                    Ok(level)
                } else {
                    Err(gpio::Error::Io(io::Error::new(io::ErrorKind::BrokenPipe, "polling on pos was interrupted")))
                }
            }) => {
                let level = pos_handle.unwrap()?;
                Ok::<(Option<Level>, Option<Level>), gpio::Error>((Some(level), None))
            }
            neg_handle = task::spawn(async move {
                let mut neg_lock: MutexGuard<InputPin> = neg.lock().unwrap();
                if let Some(level) = neg_lock.poll_interrupt(reset, None)? {
                    Ok(level)
                } else {
                    Err(gpio::Error::Io(io::Error::new(io::ErrorKind::BrokenPipe, "polling on neg was interrupted")))
                }
            }) => {
                let level = neg_handle.unwrap()?;
                Ok::<(Option<Level>, Option<Level>), gpio::Error>((None, Some(level)))
            }
        }?;

        if let (Some(pos), _) = result {
            value.0 = pos == Level::High;
        } else if let (_, Some(neg)) = result {
            value.1 = neg == Level::High;
        }

        Ok(value.into())
    }
}

/// Structure to define a home IO to handle
pub struct HomeIO {
    /// LED1 RVB
    d1: OutputLed,

    // LED2 RVB
    d2: OutputLed,

    /// Output pilote wire 1
    pil1_out: OutputPilote,
    /// Input pilote wire 1
    pil1_in: InputPilote,

    /// Output pilote wire 2
    pil2_out: OutputPilote,
    /// Input pilote wire 2
    pil2_in: InputPilote,

    /// Input pilote wire 3
    pil3_out: OutputPilote,
    /// Output pilote wire 3
    pil3_in: InputPilote,

    /// Relais on the card
    relais: OutputPin,
}

impl HomeIO {
    /// Method to create a new OVPinergy Home
    pub fn new() -> gpio::Result<HomeIO> {
        let gpio = Gpio::new()?;

        let d1 = OutputLed::new(&gpio, 24, 23, 18)?;
        let d2 = OutputLed::new(&gpio, 7, 8, 25)?;

        let mut pil1_out = OutputPilote::new(&gpio, 20, 19)?;
        pil1_out.set(Default::default());
        let pil1_in = InputPilote::new(&gpio, 21, 26)?;

        let mut pil2_out = OutputPilote::new(&gpio, 5, 0)?;
        pil2_out.set(Default::default());
        let pil2_in = InputPilote::new(&gpio, 13, 6)?;

        let mut pil3_out = OutputPilote::new(&gpio, 11, 9)?;
        pil3_out.set(Default::default());
        let pil3_in = InputPilote::new(&gpio, 27, 17)?;

        let relais = gpio.get(12)?.into_output();

        Ok(HomeIO {
            d1,
            d2,
            pil1_out,
            pil1_in,
            pil2_out,
            pil2_in,
            pil3_out,
            pil3_in,
            relais,
        })
    }

    /// Setter of the first LED color
    pub fn set_d1(&mut self, red: bool, green: bool, blue: bool) {
        self.d1.set(red, green, blue);
    }

    /// Setter of the second LED color
    pub fn set_d2(&mut self, red: bool, green: bool, blue: bool) {
        self.d2.set(red, green, blue);
    }

    /// Setter of the relais
    pub fn set_relais(&mut self, val: bool) {
        if val {
            self.relais.set_high();
        } else {
            self.relais.set_low();
        }
    }

    /// Getter of the first pilote wire
    pub fn get_pil1(&self) -> PiloteMode {
        self.pil1_in.get()
    }

    /// Getter of the second pilote wire
    pub fn get_pil2(&self) -> PiloteMode {
        self.pil2_in.get()
    }

    /// Getter of the third pilote wire
    pub fn get_pil3(&self) -> PiloteMode {
        self.pil3_in.get()
    }

    /// Setter of the first pilote wire
    pub fn set_pil1(&mut self, mode: PiloteMode) {
        self.pil1_out.set(mode)
    }

    /// Setter of the second pilote wire
    pub fn set_pil2(&mut self, mode: PiloteMode) {
        self.pil2_out.set(mode)
    }

    /// Setter of the third pilote wire
    pub fn set_pil3(&mut self, mode: PiloteMode) {
        self.pil3_out.set(mode)
    }
}
