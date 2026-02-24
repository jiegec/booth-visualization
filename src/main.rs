use yew::prelude::*;

#[derive(Clone, Copy, PartialEq)]
enum Action {
    Init,
    Add,
    Sub,
    Shift,
}

impl Action {
    fn to_string(&self) -> &'static str {
        match self {
            Action::Init => "Init",
            Action::Add => "Add",
            Action::Sub => "Sub",
            Action::Shift => "Shift",
        }
    }
}

#[derive(Clone)]
struct Step {
    step: i32,
    shift_steps: i32,
    action: Action,
    product: i32,
    additional: i32,
    bits: i32,
}

struct Model {
    a: i32,
    input_a: String,
    b: i32,
    input_b: String,
    bits: i32,
}

enum Msg {
    InputA(String),
    InputB(String),
    InputBits(String),
}

fn clamp_input(input: i32, bits: i32) -> i32 {
    let min = -(1 << bits) / 2;
    let max = (1 << bits) / 2 - 1;
    input.clamp(min, max)
}

fn get_bit(number: i32, bit: i32) -> i32 {
    if (number & (1 << bit)) != 0 { 1 } else { 0 }
}

fn shift(prev: &Step) -> Step {
    Step {
        step: prev.step + 1,
        shift_steps: prev.shift_steps + 1,
        action: Action::Shift,
        product: prev.product >> 1,
        additional: prev.product & 1,
        bits: prev.bits,
    }
}

fn check(prev: &Step, a: i32) -> Step {
    let lowbits = (prev.product & 1) * 2 + prev.additional;

    if lowbits == 2 {
        Step {
            step: prev.step + 1,
            shift_steps: prev.shift_steps,
            action: Action::Sub,
            product: prev.product - (a << prev.bits),
            additional: prev.additional,
            bits: prev.bits,
        }
    } else if lowbits == 1 {
        Step {
            step: prev.step + 1,
            shift_steps: prev.shift_steps,
            action: Action::Add,
            product: prev.product + (a << prev.bits),
            additional: prev.additional,
            bits: prev.bits,
        }
    } else {
        Step {
            step: prev.step + 1,
            shift_steps: prev.shift_steps + 1,
            action: Action::Shift,
            product: prev.product >> 1,
            additional: prev.product & 1,
            bits: prev.bits,
        }
    }
}

fn next_step(prev: &Step, a: i32) -> Step {
    match prev.action {
        Action::Add => shift(prev),
        Action::Sub => shift(prev),
        Action::Init => check(prev, a),
        Action::Shift => check(prev, a),
    }
}

fn all_steps(init_step: &Step, a: i32, bits: i32) -> Vec<Step> {
    let mut steps = vec![];
    let mut current = init_step.clone();

    while current.shift_steps < bits {
        steps.push(current.clone());
        current = next_step(&current, a);
    }
    steps.push(current);
    steps
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            a: 3,
            input_a: "3".to_string(),
            b: -7,
            input_b: "-7".to_string(),
            bits: 5,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::InputA(a) => {
                self.input_a = a.clone();
                if let Ok(val) = a.parse::<i32>() {
                    self.a = clamp_input(val, self.bits);
                }
                true
            }
            Msg::InputB(b) => {
                self.input_b = b.clone();
                if let Ok(val) = b.parse::<i32>() {
                    self.b = clamp_input(val, self.bits);
                }
                true
            }
            Msg::InputBits(input) => {
                if let Ok(val) = input.parse::<i32>() {
                    self.bits = val.clamp(1, 15);
                    self.a = clamp_input(self.a, self.bits);
                    self.b = clamp_input(self.b, self.bits);
                }
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        let init_step = Step {
            step: 0,
            shift_steps: 0,
            action: Action::Init,
            product: self.b & ((1 << self.bits) - 1),
            additional: 0,
            bits: self.bits,
        };

        let steps = all_steps(&init_step, self.a, self.bits);
        let final_answer = steps.last().map(|s| s.product).unwrap_or(0);

        html! {
            <div>
                <h1>{ "Visualization of Booth Multiplication" }</h1>

                <div>
                    { "A = " }
                    { self.a }
                    <input
                        type="text"
                        value={self.input_a.clone()}
                        oninput={link.callback(|e: InputEvent| {
                            let input: web_sys::HtmlInputElement = e.target_unchecked_into();
                            Msg::InputA(input.value())
                        })}
                    />
                </div>
                { self.show_int(self.a, self.bits) }

                <div>
                    { "B = " }
                    { self.b }
                    <input
                        type="text"
                        value={self.input_b.clone()}
                        oninput={link.callback(|e: InputEvent| {
                            let input: web_sys::HtmlInputElement = e.target_unchecked_into();
                            Msg::InputB(input.value())
                        })}
                    />
                </div>
                { self.show_int(self.b, self.bits) }

                <div>
                    { "Bits = " }
                    <input
                        type="text"
                        value={self.bits.to_string()}
                        oninput={link.callback(|e: InputEvent| {
                            let input: web_sys::HtmlInputElement = e.target_unchecked_into();
                            Msg::InputBits(input.value())
                        })}
                    />
                </div>

                { self.show_steps(&steps) }

                <p>
                    { "Answer is " }
                    { final_answer }
                </p>

                <details open=true>
                    <summary>{ "Algorithm" }</summary>
                    <ol>
                        <li>{ "`Partial Product` is initialized to `B` (zero extension)." }</li>
                        <li>{ "For each step after `Init` or `Shift`, check the LSB of `Partial Product` and `Additional Bit` to determine the next step." }</li>
                        <li>{ "`Add` or `Substract` `A` shifted by `Bits` or `Shift` again." }</li>
                        <li>{ "Repeat until `Shift` `Bits`-times." }</li>
                    </ol>
                </details>

                <details>
                    <summary>{ "Caveats" }</summary>
                    <ol>
                        <li>{ "`Partial Product` has an additional sign bit to handle `A` = -2^(n-1) case." }</li>
                        <li>{ "`Step` column is counted by the number of `Shift` steps." }</li>
                    </ol>
                </details>
            </div>
        }
    }
}

impl Model {
    fn show_int(&self, number: i32, bits: i32) -> Html {
        let bits_html: Html = (0..bits)
            .rev()
            .map(|i| {
                html! {
                    <span style="font-family: monospace;">
                        { get_bit(number, i) }
                    </span>
                }
            })
            .collect();

        html! {
            <div style="font-family: monospace;">
                { bits_html }
            </div>
        }
    }

    fn show_steps(&self, steps: &[Step]) -> Html {
        html! {
            <div>
                <table>
                    <thead>
                        <tr>
                            <th>{ "Step" }</th>
                            <th>{ "Sub Step" }</th>
                            <th>{ "Action" }</th>
                            <th>{ "Partial Product" }</th>
                            <th>{ "Additional Bit" }</th>
                        </tr>
                    </thead>
                    <tbody>
                        { for steps.iter().map(|s| self.show_step(s)) }
                    </tbody>
                </table>
            </div>
        }
    }

    fn show_step(&self, step: &Step) -> Html {
        html! {
            <tr>
                <td>{ step.shift_steps }</td>
                <td>{ step.step }</td>
                <td>{ step.action.to_string() }</td>
                <td>{ self.show_int_in_table(step.product, step.bits * 2 + 1) }</td>
                <td>{ step.additional }</td>
            </tr>
        }
    }

    fn show_int_in_table(&self, number: i32, bits: i32) -> Html {
        let bits_html: Html = (0..bits)
            .rev()
            .map(|i| {
                html! {
                    <span style="font-family: monospace;">
                        { get_bit(number, i) }
                    </span>
                }
            })
            .collect();

        html! {
            <span style="font-family: monospace;">
                { bits_html }
            </span>
        }
    }
}

fn main() {
    yew::Renderer::<Model>::new().render();
}
