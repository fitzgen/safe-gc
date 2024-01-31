use arbitrary::{Arbitrary, Result, Unstructured};
use safe_gc::{Collector, Gc, Heap, Root, Trace};

#[derive(Debug)]
pub struct Mutator(Vec<Op>);

struct A;

impl Trace for A {
    fn trace(&self, _: &mut Collector) {}
}

struct B(Option<Gc<B>>);

impl Trace for B {
    fn trace(&self, collector: &mut Collector) {
        if let Some(x) = self.0 {
            collector.edge(x);
        }
    }
}

struct C {
    x: Option<Gc<C>>,
    y: Option<Gc<D>>,
}

impl Trace for C {
    fn trace(&self, collector: &mut Collector) {
        if let Some(x) = self.x {
            collector.edge(x);
        }
        if let Some(y) = self.y {
            collector.edge(y);
        }
    }
}

struct D(Vec<Gc<C>>);

impl Trace for D {
    fn trace(&self, collector: &mut Collector) {
        for c in &self.0 {
            collector.edge(*c);
        }
    }
}

#[derive(Debug)]
enum Op {
    Gc,

    AllocA,
    AllocB(Option<usize>),
    AllocC { x: Option<usize>, y: Option<usize> },
    AllocD(Vec<usize>),

    SetB(usize, Option<usize>),
    SetCX(usize, Option<usize>),
    SetCY(usize, Option<usize>),
    SetD(usize, usize, usize),

    UnrootA(usize),
    UnrootB(usize),
    UnrootC(usize),
    UnrootD(usize),
}

impl<'a> Arbitrary<'a> for Mutator {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let mut ops = vec![];

        #[derive(Default)]
        struct State {
            a_count: usize,
            b_count: usize,
            c_count: usize,
            d_count: usize,
        }

        let mut state = State::default();
        let mut choices: Vec<fn(&mut Unstructured, &mut State) -> Result<Op>> = vec![];
        while !u.is_empty() {
            choices.clear();

            choices.push(|_, _| Ok(Op::Gc));

            choices.push(|_, state| {
                state.a_count += 1;
                Ok(Op::AllocA)
            });

            choices.push(|u, state| {
                let op = Op::AllocB(if state.b_count > 0 && u.arbitrary()? {
                    Some(u.int_in_range(0..=state.b_count - 1)?)
                } else {
                    None
                });
                state.b_count += 1;
                Ok(op)
            });

            choices.push(|u, state| {
                let x = if state.c_count > 0 && u.arbitrary()? {
                    Some(u.int_in_range(0..=state.c_count - 1)?)
                } else {
                    None
                };
                let y = if state.d_count > 0 && u.arbitrary()? {
                    Some(u.int_in_range(0..=state.d_count - 1)?)
                } else {
                    None
                };
                state.c_count += 1;
                Ok(Op::AllocC { x, y })
            });

            choices.push(|u, state| {
                state.d_count += 1;
                let mut cs = vec![];
                if state.c_count > 0 {
                    for _ in 0..u.int_in_range::<u8>(0..=8)? {
                        cs.push(u.int_in_range(0..=state.c_count - 1)?);
                    }
                }
                Ok(Op::AllocD(cs))
            });

            if state.a_count > 0 {
                choices.push(|u, state| {
                    let op = Op::UnrootA(u.int_in_range(0..=state.a_count - 1)?);
                    state.a_count -= 1;
                    Ok(op)
                });
            }

            if state.b_count > 0 {
                choices.push(|u, state| {
                    Ok(Op::SetB(
                        u.int_in_range(0..=state.b_count - 1)?,
                        if u.arbitrary()? {
                            Some(u.int_in_range(0..=state.b_count - 1)?)
                        } else {
                            None
                        },
                    ))
                });
                choices.push(|u, state| {
                    let op = Op::UnrootB(u.int_in_range(0..=state.b_count - 1)?);
                    state.b_count -= 1;
                    Ok(op)
                });
            }

            if state.c_count > 0 {
                choices.push(|u, state| {
                    Ok(Op::SetCX(
                        u.int_in_range(0..=state.c_count - 1)?,
                        if u.arbitrary()? {
                            Some(u.int_in_range(0..=state.c_count - 1)?)
                        } else {
                            None
                        },
                    ))
                });
                choices.push(|u, state| {
                    Ok(Op::SetCY(
                        u.int_in_range(0..=state.c_count - 1)?,
                        if state.d_count > 0 && u.arbitrary()? {
                            Some(u.int_in_range(0..=state.d_count - 1)?)
                        } else {
                            None
                        },
                    ))
                });
                choices.push(|u, state| {
                    let op = Op::UnrootC(u.int_in_range(0..=state.c_count - 1)?);
                    state.c_count -= 1;
                    Ok(op)
                });
            }

            if state.d_count > 0 {
                if state.c_count > 0 {
                    choices.push(|u, state| {
                        Ok(Op::SetD(
                            u.int_in_range(0..=state.d_count - 1)?,
                            u.arbitrary()?,
                            u.int_in_range(0..=state.c_count - 1)?,
                        ))
                    });
                }
                choices.push(|u, state| {
                    let op = Op::UnrootD(u.int_in_range(0..=state.d_count - 1)?);
                    state.d_count -= 1;
                    Ok(op)
                });
            }

            let f = u.choose(&choices)?;
            let op = f(u, &mut state)?;
            ops.push(op);
        }

        Ok(Mutator(ops))
    }
}

impl Mutator {
    pub fn run(&self) {
        self.run_in(&mut Heap::new());
    }

    pub fn run_in(&self, heap: &mut Heap) {
        let mut a_roots: Vec<Root<A>> = vec![];
        let mut b_roots: Vec<Root<B>> = vec![];
        let mut c_roots: Vec<Root<C>> = vec![];
        let mut d_roots: Vec<Root<D>> = vec![];

        for op in &self.0 {
            match op {
                Op::Gc => heap.gc(),

                Op::AllocA => a_roots.push(heap.alloc(A)),
                Op::AllocB(b) => b_roots.push(heap.alloc(B(b.map(|b| b_roots[b].unrooted())))),
                Op::AllocC { x, y } => c_roots.push(heap.alloc(C {
                    x: x.map(|x| c_roots[x].unrooted()),
                    y: y.map(|y| d_roots[y].unrooted()),
                })),
                Op::AllocD(cs) => {
                    d_roots.push(heap.alloc(D(cs.iter().map(|c| c_roots[*c].unrooted()).collect())))
                }

                Op::SetB(b, c) => {
                    let c = c.map(|c| b_roots[c].unrooted());
                    heap[&b_roots[*b]].0 = c;
                }
                Op::SetCX(c, x) => {
                    let x = x.map(|x| c_roots[x].unrooted());
                    heap[&c_roots[*c]].x = x;
                }
                Op::SetCY(c, y) => {
                    let y = y.map(|y| d_roots[y].unrooted());
                    heap[&c_roots[*c]].y = y;
                }
                Op::SetD(d, i, c) => {
                    let c = c_roots[*c].unrooted();
                    let d = &mut heap[&d_roots[*d]];
                    if d.0.len() == 0 {
                        d.0.push(c);
                    } else {
                        let i = i % d.0.len();
                        d.0[i] = c;
                    }
                }

                Op::UnrootA(a) => {
                    a_roots.swap_remove(*a);
                }
                Op::UnrootB(b) => {
                    b_roots.swap_remove(*b);
                }
                Op::UnrootC(c) => {
                    c_roots.swap_remove(*c);
                }
                Op::UnrootD(d) => {
                    d_roots.swap_remove(*d);
                }
            }
        }
    }
}
