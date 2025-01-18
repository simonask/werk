fn main() {
    shadow_rs::ShadowBuilder::builder()
        .build_pattern(shadow_rs::BuildPattern::Lazy)
        .build()
        .unwrap();
}
