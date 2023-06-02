import { screen } from "@testing-library/dom";
import userEvent from "@testing-library/user-event";
import m from "mithril";

import myapp from "./public/myapp";
const { exports_for_js } = myapp;
const {
  mountApp,
  fact,
  getBoolFromJs,
  maxViaJsInline,
  runAllTestsJest,
} = exports_for_js();

test("run all mini-fiveam tests", () => {
  const [result, report] = runAllTestsJest();
  console.log(report);
  expect(result).toBe(true);
});

test("uses jest-dom", async () => {
  const user = userEvent.setup();
  window.document.body.innerHTML = '<div id="app"></div>';

  mountApp();

  expect(
    screen.getByRole("heading", { name: "Othello Square" })
  ).toBeInTheDocument();

  const nicknameInput = document.getElementById("nickname");
  await user.type(nicknameInput, "Peter");
  screen.getByRole("button", { name: "Login" }).click();
  m.redraw.sync();

  expect(
    screen.getByTestId("message", { name: "Cheesecakes: 1" })
  ).toBeInTheDocument();
});

test("fact", () => {
  expect(fact(5)).toBe(120);
  expect(fact(6)).toBe(720);
  expect(fact(7)).toBe(5040);
  expect(fact(8)).toBe(40320);
});

test("getBoolFromJs", () => {
  expect(getBoolFromJs(1)).toBe(1);
  expect(getBoolFromJs(2)).toBe(2);
});

test("maxViaJsInline", () => {
  expect(maxViaJsInline(1, 2)).toBe(2);
  expect(maxViaJsInline(1, 1)).toBe(1);
  expect(maxViaJsInline(200, 100)).toBe(200);
});
