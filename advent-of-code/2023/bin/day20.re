include Lib.Util;

type module_type =
  | Broadcast
  | FlipFlop
  | Conjunction;

type signal =
  | High
  | Low;

let invert =
  fun
  | Low => High
  | High => Low;

type modules = Hashtbl.t(string, module_type);

type state = Hashtbl.t(string, signal);

type cables = Hashtbl.t(string, list(string));

type memory = Hashtbl.t((string, string), signal);

type network = {
  modules,
  cables,
  inverse_cables: cables,
  state,
  memory,
};

let extract_type = source => {
  let name = String.sub(source, 1, String.length(source) - 1);

  switch (source.[0]) {
  | 'b' => (Broadcast, source)
  | '%' => (FlipFlop, name)
  | '&' => (Conjunction, name)
  | _ => assert(false)
  };
};

let cable_delimiter = " -> " |> Str.regexp_string;

let destination_delimiter = ", " |> Str.regexp_string;

let parse_input = lines => {
  let n = List.length(lines);

  let modules = Hashtbl.create(n);
  let cables = Hashtbl.create(n);
  let inverse_cables = Hashtbl.create(n);
  let state = Hashtbl.create(n);
  let memory = Hashtbl.create(n);

  let add_edge = (m, source, destination) => {
    let edges = Hashtbl.find_opt(m, source) |> Option.value(~default=[]);

    Hashtbl.replace(m, source, [destination, ...edges]);
  };

  List.iter(
    line => {
      switch (Str.split(cable_delimiter, line)) {
      | [source, destination] =>
        let (module_type, module_name) = extract_type(source);
        Hashtbl.replace(modules, module_name, module_type);

        let destinations = Str.split(destination_delimiter, destination);

        List.iter(
          destination => {
            add_edge(cables, module_name, destination);
            add_edge(inverse_cables, destination, module_name);
          },
          destinations,
        );
      | _ => assert(false)
      }
    },
    lines,
  );

  Hashtbl.to_seq(cables)
  |> Seq.map(((key, value)) => (key, List.rev(value)))
  |> Hashtbl.replace_seq(cables);

  {modules, cables, inverse_cables, state, memory};
};

let tx = "broadcaster";
let rx = "rx";

let send_pulse = (rx_conjunction, rx_parents, network) => {
  let queue = Queue.create();
  Queue.add((tx, Low), queue);

  let add_to_queue = (source, destination, signal) => {
    let module_type = Hashtbl.find_opt(network.modules, destination);
    if (module_type == Some(Conjunction)) {
      Hashtbl.replace(network.memory, (source, destination), signal);
    };

    Queue.add((destination, signal), queue);
  };

  let low_pulses = ref(0);
  let high_pulses = ref(0);

  let parents_on = ref(None);

  while (!Queue.is_empty(queue)) {
    let (node, signal) = Queue.take(queue);
    switch (signal) {
    | Low => low_pulses := low_pulses^ + 1
    | High => high_pulses := high_pulses^ + 1
    };

    if (node == rx) {
      let on =
        List.filter(
          parent =>
            Hashtbl.find(network.memory, (parent, rx_conjunction)) == High,
          rx_parents,
        );

      if (on != []) {
        parents_on := Some(on);
      };
    };

    let module_type = Hashtbl.find_opt(network.modules, node);
    let edges =
      Hashtbl.find_opt(network.cables, node) |> Option.value(~default=[]);

    switch (module_type) {
    | Some(Broadcast) =>
      List.iter(
        destination => add_to_queue(node, destination, signal),
        edges,
      )
    | Some(FlipFlop) =>
      let state =
        Hashtbl.find_opt(network.state, node) |> Option.value(~default=Low);
      if (signal == Low) {
        let state = invert(state);
        Hashtbl.replace(network.state, node, state);

        List.iter(
          destination => add_to_queue(node, destination, state),
          edges,
        );
      };
    | Some(Conjunction) =>
      let current_state = {
        let inputs = Hashtbl.find(network.inverse_cables, node);
        let input_states =
          List.map(
            input =>
              Hashtbl.find_opt(network.memory, (input, node))
              |> Option.value(~default=Low),
            inputs,
          );

        List.for_all(state => state == High, input_states) ? Low : High;
      };

      List.iter(
        destination => add_to_queue(node, destination, current_state),
        edges,
      );
    | None => ()
    };
  };

  (low_pulses^, high_pulses^, parents_on^);
};

let solve = input => {
  let low_pulses = ref(0);
  let high_pulses = ref(0);

  let rx_parents = Hashtbl.find(input.inverse_cables, rx);
  assert(List.length(rx_parents) == 1);

  let rx_conjunction = List.hd(rx_parents);
  assert(Hashtbl.find(input.modules, rx_conjunction) == Conjunction);

  let rx_parents =
    List.concat_map(
      parent => Hashtbl.find(input.inverse_cables, parent),
      rx_parents,
    );

  let send_pulse = send_pulse(rx_conjunction, rx_parents);

  let cycles_amt = List.length(rx_parents);
  let cycles = Hashtbl.create(cycles_amt);

  let iters = ref(0);
  while (Hashtbl.length(cycles) != cycles_amt) {
    iters := iters^ + 1;

    let (low, high, parents_on) = send_pulse(input);

    if (iters^ <= 1000) {
      low_pulses := low_pulses^ + low;
      high_pulses := high_pulses^ + high;
    };

    let parents_on = Option.value(~default=[], parents_on);

    List.to_seq(parents_on)
    |> Seq.filter(Hashtbl.mem(cycles) >> (!))
    |> Seq.map(parent => (parent, iters^))
    |> Hashtbl.add_seq(cycles);
  };

  let part1 = low_pulses^ * high_pulses^;
  let part2 = Hashtbl.to_seq_values(cycles) |> Seq.fold_left(lcm, 1);

  (part1, part2);
};

let () = {
  let input = read_lines("inputs/day20.txt") |> List.of_seq |> parse_input;

  let (part1, part2) = solve(input);
  Printf.printf("%d, %d\n", part1, part2);
};
