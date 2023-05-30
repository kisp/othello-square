import { screen } from "@testing-library/dom";
import m from "mithril";

import myapp from "./myapp";
const { exports_for_js } = myapp;
const { myinit, fact, getBoolFromJs, maxViaJsInline } = exports_for_js();

test("uses jest-dom", () => {
  window.document.body.innerHTML = '<div id="app"></div>';

  myinit();

  expect(
    screen.getByRole("heading", { name: "Cheesecakes: 0" })
  ).toBeInTheDocument();

  screen.getByRole("button", { name: "Add cheesecake" }).click();
  m.redraw.sync();

  expect(
    screen.getByRole("heading", { name: "Cheesecakes: 1" })
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
