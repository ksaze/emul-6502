#[allow(non_snake_case)]
pub struct Signals {
    pub RES_sync: bool,
    pub IRQ_sync: bool,
    pub NMIP: bool,

    pub RESP: bool,
    pub IRQP: bool,

    pub NMIL: bool,
    pub sig_1368: bool,

    pub doIRQ: bool,
    pub NMIG: bool,
    pub RESG: bool,

    pub INTG: bool,
    pub D1x1: bool,

    pub VEC: bool,
    pub brk_done: bool,

    // Internal emulator signals
    pub poll_int: bool,
    pub in_reset: bool,
    pub res_hijack: bool,
    pub NMIP_ph1: bool,
    pub branch_T3: bool,
    pub VEC_next_cycle: bool,
}

fn bool_str(val: bool) -> &'static str {
    if val { "1" } else { "0" }
}

impl Signals {
    pub fn new() -> Self {
        Self {
            RES_sync: true,
            IRQ_sync: false,
            RESP: true,
            NMIP: false,
            IRQP: false,
            NMIL: false,
            sig_1368: false,
            RESG: true, // RESET on power-on
            NMIG: false,
            INTG: false,
            doIRQ: false,
            D1x1: true,
            poll_int: false,
            VEC: false,
            brk_done: false,
            in_reset: true, // RESET on power-on (skip pin down cycles)
            NMIP_ph1: false,
            branch_T3: false,
            res_hijack: false,
            VEC_next_cycle: false,
        }
    }

    pub fn ph1(&mut self, irq_disable: bool) {
        self.RESP = !self.RES_sync;
        // Ignore RES clear during res_hijack
        // res_hijack cleared in fetch following full hijack before this fn is called
        if !self.res_hijack {
            self.RESG &= !self.brk_done;
        }
        self.D1x1 = !self.RESG & !self.INTG;

        self.NMIG = self.sig_1368 | (self.NMIG & !self.brk_done);
        self.NMIL = self.NMIP_ph1;
        self.sig_1368 = self.NMIP & !self.NMIL & !self.VEC;
        self.NMIP_ph1 = self.NMIP && !self.VEC;

        self.IRQP = self.IRQ_sync;

        self.doIRQ = self.NMIG | (self.IRQP & !irq_disable);

        self.VEC = self.VEC_next_cycle;
    }

    pub fn ph2(&mut self) {
        // valid for vec_hi.ph2, but not vec_lo.ph2--which is the same as brk-done
        // post operation completion
        // self.sig_1368 = self.NMIP & !self.NMIL & !self.brk_done;
        self.sig_1368 = self.NMIP & !self.NMIL & !self.VEC;

        // before operation microp
        self.RESG |= self.RESP;
        self.INTG = (!self.poll_int && self.INTG) || (self.poll_int && self.doIRQ);

        self.D1x1 = !self.RESG & !self.INTG;

        self.poll_int = false;
    }

    #[cfg(feature = "test-utils")]
    pub fn trace(&self) -> String {
        format!(
            "IRQ_sync = {}, RES_sync = {} NMIP = {} IRQP = {} RESP = {}\n1368={} NMIG={} INTG={} RESG={}\nD1x1={} VEC={} brk_done={}",
            bool_str(self.IRQ_sync),
            bool_str(self.RES_sync),
            bool_str(self.NMIP),
            bool_str(self.IRQP),
            bool_str(self.RESP),
            bool_str(self.sig_1368),
            bool_str(self.NMIG),
            bool_str(self.INTG),
            bool_str(self.RESG),
            bool_str(self.D1x1),
            bool_str(self.VEC),
            bool_str(self.brk_done),
        )
    }
}
