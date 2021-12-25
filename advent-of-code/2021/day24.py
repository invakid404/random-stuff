import z3

with open("input.txt", "r") as f:
    lines = f.readlines()

instructions = [line.strip().split(" ") for line in lines]

solver = z3.Optimize()

digits = [z3.BitVec(f"digit_{i}", 64) for i in range(14)]
for d in digits:
    solver.add(1 <= d)
    solver.add(d <= 9)

digit_it = iter(digits)

zero, one = z3.BitVecVal(0, 64), z3.BitVecVal(1, 64)
registers = {register: zero for register in "xyzw"}

for idx, instruction in enumerate(instructions):
    if instruction[0] == "inp":
        registers[instruction[1]] = next(digit_it)

        continue

    a, b = instruction[1:]
    b = registers[b] if b in registers else int(b)

    c = z3.BitVec(f"value_{idx}", 64)

    if instruction[0] == "add":
        solver.add(c == registers[a] + b)
    elif instruction[0] == "mul":
        solver.add(c == registers[a] * b)
    elif instruction[0] == "mod":
        solver.add(registers[a] >= 0)
        solver.add(b > 0)

        solver.add(c == registers[a] % b)
    elif instruction[0] == "div":
        solver.add(b != 0)

        solver.add(c == registers[a] / b)
    elif instruction[0] == "eql":
        solver.add(c == z3.If(registers[a] == b, one, zero))
    else:
        assert False

    registers[a] = c


solver.add(registers["z"] == 0)


for fn in (solver.maximize, solver.minimize):
    solver.push()

    fn(sum((10 ** idx) * digit for idx, digit in enumerate(digits[::-1])))

    solver.check()
    model = solver.model()

    print("".join(str(model[digit]) for digit in digits))

    solver.pop()
