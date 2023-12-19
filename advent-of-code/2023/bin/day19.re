include Lib.Util;

type part = Hashtbl.t(string, int);

type transition =
  | Accepted
  | Rejected
  | Workflow(string);

type eval_state = transition;

type operator =
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual;

type condition = {
  variable: string,
  operator,
  target: int,
};

type rule =
  | Pass(transition)
  | Predicate(condition, transition);

type workflow = list(rule);

type input = {
  workflows: Hashtbl.t(string, workflow),
  parts: list(part),
};

let condition_pattern = {|\(.+\)\(<\|>\)\([0-9]+\)|} |> Str.regexp;

let parse_condition = condition => {
  assert(Str.string_match(condition_pattern, condition, 0));

  let variable = Str.matched_group(1, condition);
  let operator = Str.matched_group(2, condition);
  let target = Str.matched_group(3, condition) |> int_of_string;

  let operator =
    switch (operator) {
    | "<" => LessThan
    | ">" => GreaterThan
    | _ => assert(false)
    };

  {variable, operator, target};
};

let parse_transition =
  fun
  | "A" => Accepted
  | "R" => Rejected
  | workflow => Workflow(workflow);

let parse_predicate = rule =>
  switch (String.split_on_char(':', rule)) {
  | [condition, transition] =>
    Predicate(condition |> parse_condition, transition |> parse_transition)
  | _ => assert(false)
  };

let parse_rule = rule =>
  String.contains(rule, ':')
    ? parse_predicate(rule) : Pass(rule |> parse_transition);

let parse_rules = String.split_on_char(',') >> List.map(parse_rule);

let workflow_pattern = {|\(.*\){\(.*\)}|} |> Str.regexp;

let parse_workflow = line => {
  assert(Str.string_match(workflow_pattern, line, 0));
  let name = Str.matched_group(1, line);
  let rules = Str.matched_group(2, line);

  (name, parse_rules(rules));
};

let parse_workflows =
  String.split_on_char('\n')
  >> List.to_seq
  >> Seq.map(parse_workflow)
  >> (
    entries => {
      let result = Hashtbl.create(Seq.length(entries));
      Hashtbl.add_seq(result, entries);

      result;
    }
  );

let parse_part = part => {
  let part = String.sub(part, 1, String.length(part) - 2);
  let vars = String.split_on_char(',', part);

  let result = Hashtbl.create(List.length(vars));

  List.to_seq(vars)
  |> Seq.map(var => {
       switch (String.split_on_char('=', var)) {
       | [name, value] => (name, int_of_string(value))
       | _ => assert(false)
       }
     })
  |> Hashtbl.add_seq(result);

  result;
};

let parse_parts = String.split_on_char('\n') >> List.map(parse_part);

let input_delimiter = "\n\n" |> Str.regexp_string;

let parse_input = input =>
  switch (Str.split(input_delimiter, input)) {
  | [workflows, parts] => {
      workflows: parse_workflows(workflows),
      parts: parse_parts(parts),
    }
  | _ => assert(false)
  };

let evaluate_condition = (condition, part) => {
  let variable = Hashtbl.find(part, condition.variable);

  let operator =
    switch (condition.operator) {
    | LessThan => (<)
    | GreaterThan => (>)
    | LessThanOrEqual => (<=)
    | GreaterThanOrEqual => (>=)
    };

  operator(variable, condition.target);
};

let evaluate_rule = (rule, part) =>
  switch (rule) {
  | Pass(transition) => Some(transition)
  | Predicate(condition, transition) =>
    evaluate_condition(condition, part) ? Some(transition) : None
  };

let execute_workflow = (workflow, part) =>
  List.filter_map(rule => evaluate_rule(rule, part), workflow) |> List.hd;

let evaluate = (workflows, part) => {
  let current_state = Workflow("in") |> ref;
  let result = ref(None);

  while (Option.is_none(result^)) {
    switch (current_state^) {
    | Workflow(workflow_name) =>
      let workflow = Hashtbl.find(workflows, workflow_name);
      current_state := execute_workflow(workflow, part);
    | final_state => result := Some(final_state)
    };
  };

  result^ |> Option.get;
};

let part1 = input =>
  input.parts
  |> List.filter(part => evaluate(input.workflows, part) == Accepted)
  |> List.map(Hashtbl.to_seq_values >> Seq.fold_left((+), 0))
  |> List.fold_left((+), 0);

let negate_condition = condition => {
  let operator =
    switch (condition.operator) {
    | LessThan => GreaterThanOrEqual
    | GreaterThan => LessThanOrEqual
    | LessThanOrEqual => GreaterThan
    | GreaterThanOrEqual => LessThan
    };

  {...condition, operator};
};

let acceptance_conditions = workflows => {
  let queue = Queue.create();
  Queue.add((Workflow("in"), () => Seq.Nil), queue);

  let accepted = ref([]);

  while (!Queue.is_empty(queue)) {
    let (state, conditions) = Queue.pop(queue);
    let current_conditions = ref(conditions);

    switch (state) {
    | Accepted => accepted := [conditions |> List.of_seq, ...accepted^]
    | Rejected => ()
    | Workflow(workflow_name) =>
      let workflow = Hashtbl.find(workflows, workflow_name);
      List.iter(
        rule => {
          switch (rule) {
          | Predicate(condition, transition) =>
            let next_conditions = Seq.cons(condition, current_conditions^);
            current_conditions :=
              Seq.cons(negate_condition(condition), current_conditions^);

            Queue.add((transition, next_conditions), queue);
          | Pass(transition) =>
            Queue.add((transition, current_conditions^), queue)
          }
        },
        workflow,
      );
    };
  };

  accepted^;
};

let default_range = (1, 4000);

let accepted_range = conditions => {
  let state = Hashtbl.create(4);
  ["x", "m", "a", "s"]
  |> List.to_seq
  |> Seq.map(key => (key, default_range))
  |> Hashtbl.add_seq(state);

  let merge = ((left_min, left_max), (right_min, right_max)) => (
    max(left_min, right_min),
    min(left_max, right_max),
  );

  List.iter(
    condition => {
      let value = Hashtbl.find(state, condition.variable);
      let (min_value, max_value) = value;

      let new_value =
        switch (condition.operator) {
        | LessThan => (min_value, condition.target - 1)
        | GreaterThan => (condition.target + 1, max_value)
        | LessThanOrEqual => (min_value, condition.target)
        | GreaterThanOrEqual => (condition.target, max_value)
        };

      Hashtbl.replace(state, condition.variable, merge(value, new_value));
    },
    conditions,
  );

  Hashtbl.to_seq_values(state)
  |> Seq.map(((min_value, max_value)) => max_value - min_value + 1)
  |> Seq.fold_left(( * ), 1);
};

let part2 = input => {
  let condition_options = acceptance_conditions(input.workflows);

  List.map(accepted_range, condition_options) |> List.fold_left((+), 0);
};

let () = {
  let input = read_all("inputs/day19.txt") |> String.trim |> parse_input;

  Printf.printf("%d, %d\n", part1(input), part2(input));
};
