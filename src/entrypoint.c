// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.

void R_init_rsimsimd_extendr(void *dll);

void R_init_rsimsimd(void *dll) {
    R_init_rsimsimd_extendr(dll);
}
