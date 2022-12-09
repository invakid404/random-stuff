/** @type {import('ts-jest').JestConfigWithTsJest} */
module.exports = {
  transform: {
    "^.+\\.m?[tj]sx?$": [
      "ts-jest",
      {
        compiler: "ttypescript",
      },
    ],
  },
  setupFiles: ["<rootDir>/jest.setup.ts"],
};
