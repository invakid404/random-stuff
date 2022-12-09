import {createMock} from "ts-auto-mock";

interface Test {
	hello: string
}

describe("dummy", () => {
	let testMock: Test

	beforeEach(() => {
		testMock = createMock<Test>()
	})

	it("works", () => {
		console.log(testMock)
		expect(typeof testMock.hello).toBe("string")
	})
})
