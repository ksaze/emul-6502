#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RdyResumeState {
    Fetch,
    Exec,
    Jammed,
    Reset,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CPUState {
    Fetch,
    Exec,
    Blocked(RdyResumeState),
    Jammed,
    Reset,
}

impl CPUState {
    pub fn block(self) -> CPUState {
        let resume = match self {
            CPUState::Fetch => RdyResumeState::Fetch,
            CPUState::Exec => RdyResumeState::Exec,
            CPUState::Jammed => RdyResumeState::Jammed,
            CPUState::Reset => RdyResumeState::Reset,
            CPUState::Blocked(_) => panic!("Tried to block an already blocked CPU"),
        };
        CPUState::Blocked(resume)
    }

    pub fn unblock(self) -> CPUState {
        match self {
            CPUState::Blocked(resume) => match resume {
                RdyResumeState::Fetch => CPUState::Fetch,
                RdyResumeState::Exec => CPUState::Exec,
                RdyResumeState::Jammed => CPUState::Jammed,
                RdyResumeState::Reset => CPUState::Reset,
            },
            _ => panic!("Tried to unblock a CPU that wasn't blocked"),
        }
    }
}
