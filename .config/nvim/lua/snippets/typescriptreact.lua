local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node

return {
  -- ESLint disable next line
  s("esld", {
    t("// eslint-disable-next-line "),
    i(1, "rule-name"),
  }),
  
  -- ESLint disable next line with common rules
  s("eslint", {
    t("// eslint-disable-next-line "),
    c(1, {
      t("no-unused-vars"),
      t("@typescript-eslint/no-unused-vars"),
      t("no-console"),
      t("@typescript-eslint/no-explicit-any"),
      t("@typescript-eslint/ban-ts-comment"),
      t("react-hooks/exhaustive-deps"),
      t("no-undef"),
      i(nil, "custom-rule"),
    }),
  }),
  
  -- ESLint disable block
  s("eslb", {
    t("/* eslint-disable "),
    i(1, "rule-name"),
    t(" */"),
    t({"", ""}),
    i(2),
    t({"", "/* eslint-enable "),
    ls.f(function(args) return args[1][1] end, {1}),
    t(" */"),
  }),
  
  -- TSLint/TypeScript specific disable
  s("tsd", {
    t("// @ts-ignore"),
  }),
  
  -- TS expect error
  s("tse", {
    t("// @ts-expect-error"),
  }),
  
  -- TS no check
  s("tsn", {
    t("// @ts-nocheck"),
  }),
}
