use std::{thread::sleep, time::Duration};

use rppal::gpio;
use rs_ovpinergy::homeio::HomeIO;

fn main() -> gpio::Result<()> {
    let mut home_io = HomeIO::new()?;

    println!("Red LED D1");
    home_io.set_d1(true, false, false);

    println!("Green LED D2");
    home_io.set_d2(false, true, false);

    sleep(Duration::from_secs(10));

    println!("Blue LED D1");
    home_io.set_d1(false, false, true);

    println!("Red LED D2");
    home_io.set_d2(true, false, false);

    sleep(Duration::from_secs(10));

    println!("Light the LED D1");
    home_io.set_d1(true, true, true);

    println!("Light the LED D2");
    home_io.set_d2(true, true, true);

    sleep(Duration::from_secs(10));

    println!("Shutdown the LED D1");
    home_io.set_d1(false, false, false);

    println!("Shutdown the LED D2");
    home_io.set_d2(false, false, false);

    sleep(Duration::from_secs(10));

    Ok(())
}
