# 6502 Emulator in Rust

/tests/singlestep.rs uses [SingleStepTests](https://github.com/SingleStepTests/ProcessorTests/tree/main) for testing individual instructions.
Variant NMOS_6502 passed all documented opcodes tests.

./tests/klaus_functional.rs uses [Klaus Functional Tests](https://github.com/Klaus2m5/6502_65C02_functional_tests).
Variant NMOS_6502 passed functional test & decimal mode tests.

All Interrupt Handling Quirks Implemented:
NMI Hijacking (Full & Failed Half)
Lost NMI
RES Hijacking (Half & Thwarted Full)
IRQ delay on branch
Delayed IRQ response on CLI/SEI/PLP
Minimum one instruction gap between interrupt servicing
(Each of these quirks are tested by ./tests/interrupt_servicing.rs)

## References
- MCS 6500 Microcomputer Family Programming Manual
- https://llx.com/Neil/a2/opcodes.html
- https://6502.co.uk/course/build-a-6502-based-computer
- https://www.ahl27.com/posts/2023/01/6502-emu1/
- https://github.com/mre/mos6502/tree/master?tab=readme-ov-file
- https://www.nesdev.org/obelisk-6502-guide/addressing.html
- https://www.nesdev.org/wiki/CPU_interrupts
