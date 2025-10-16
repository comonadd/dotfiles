local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

return {
  -- @inject decorator with import
  s("@inject", {
    t("from dependency_injector.wiring import inject"),
    t({"", "", ""}),
    t("@inject"),
    t({"", ""}),
    t("def "),
    i(1, "function_name"),
    t("("),
    i(2, "args"),
    t("):"),
    t({"", "    "}),
    i(0),
  }),

  -- Just the inject import
  s("inject_import", {
    t("from dependency_injector.wiring import inject"),
  }),

  -- Just @inject decorator (assumes import exists)
  s("inject_decorator", {
    t("@inject"),
  }),

  -- Provide decorator with import
  s("@provide", {
    t("from dependency_injector.wiring import Provide, inject"),
    t({"", "", ""}),
    t("@inject"),
    t({"", ""}),
    t("def "),
    i(1, "function_name"),
    t("("),
    i(2, "param"),
    t(": "),
    i(3, "Type"),
    t(" = Provide["),
    i(4, "Container.provider"),
    t("]):"),
    t({"", "    "}),
    i(0),
  }),
}
