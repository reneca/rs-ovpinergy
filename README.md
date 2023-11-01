# rs-ovpinergy

[<img alt="github" src="https://img.shields.io/badge/github-reneca/rs--ovpinergy-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/reneca/rs-ovpinergy)
[<img alt="crates.io" src="https://img.shields.io/crates/v/rs-ovpinergy.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/rs-ovpinergy)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ovpinergy-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/rs-ovpinergy)

This Rust library is use to handle an [OVPinergy](https://www.overware.fr/ovpinergy/) card.

## Examples

There is several example available to test the card.

### Leds

This a a simple test to light the leds on the card:

```
cargo run --example leds
```

### Consumption (Amps clamp)

To test the first amp meter, there is a consumption example:

```
cargo run --example consumption
```
