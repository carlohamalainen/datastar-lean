import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  use: {
    baseURL: "http://127.0.0.1:3113",
  },
  webServer: {
    command: "lake exe e2e-server",
    cwd: "..",
    url: "http://127.0.0.1:3113",
    reuseExistingServer: !process.env.CI,
  },
});
