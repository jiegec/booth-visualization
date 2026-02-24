# booth-visualization

A visualization of booth multiplication written in Rust with Yew.

## Development

Install dependencies:
- [Rust](https://rustup.rs/)
- [Trunk](https://trunkrs.dev/): `cargo install --locked trunk`
- WASM target: `rustup target add wasm32-unknown-unknown`

Run development server:
```bash
trunk serve
```

Open http://localhost:8080 in your browser.

## Build

Build for production:
```bash
trunk build --release
```

The output will be in the `dist/` directory.

## Deployment

The project is automatically deployed to GitHub Pages on every push to the master branch.
