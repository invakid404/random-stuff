import path from "node:path";
import assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { Project, SourceFile, TypeFormatFlags } from "ts-morph";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const evaluateType = (sourceFile: SourceFile, name: string, input: string) => {
  const evalType = sourceFile.addTypeAlias({
    name: `$Eval${name}`,
    type: `${name}<${JSON.stringify(input)}>`,
  });

  return evalType
    .getType()
    .getText(
      undefined,
      TypeFormatFlags.NoTruncation | TypeFormatFlags.InTypeAlias,
    );
};

const evaluate = async (solutionPath: string, input: string) => {
  const configPath = path.join(__dirname, "tsconfig.json");

  const project = new Project({
    tsConfigFilePath: configPath,
  });

  const sourceFile = project.getSourceFile(solutionPath);
  assert(sourceFile != null);

  const part1 = evaluateType(sourceFile, "Part1", input);
  const part2 = evaluateType(sourceFile, "Part2", input);

  console.log("Part 1:", part1);
  console.log("Part 2:", part2);
};

const exists = (path: string) =>
  fs.stat(path).then(
    () => true,
    () => false,
  );

const run = async (day: number) => {
  const baseName = `day${day.toString().padStart(2, "0")}`;

  const solutionFile = path.join(__dirname, `solutions/${baseName}.ts`);
  assert(await exists(solutionFile), `Solution for day ${day} doesn't exist`);

  const inputFile = path.join(__dirname, `inputs/${baseName}.txt`);
  assert(await exists(inputFile), `Input for day ${day} doesn't exist`);

  const input = (await fs.readFile(inputFile, "utf-8")).trim();

  return evaluate(solutionFile, input);
};

const args = process.argv.slice(2);
assert(args.length === 1, "No day provided");

const day = parseInt(args[0], 10);
assert(!isNaN(day), "Day must be a number");

await run(day);