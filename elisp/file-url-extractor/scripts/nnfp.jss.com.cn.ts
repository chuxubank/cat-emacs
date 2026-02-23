import { chromium } from "playwright";

const url = process.argv[2];
const fileExt = process.argv[3];

const browser = await chromium.launch();
const page = await browser.newPage();

await page.goto(url, { waitUntil: "networkidle" });

const state = await page.evaluate(() =>
  localStorage.getItem("state")
);

const parsed = JSON.parse(state!);

if (fileExt === "pdf") {
  console.log(
    parsed.invoicePreviewData.invoiceSimpleVo.url
  );
} else {
  console.error("Unsupported file type:", fileExt);
  process.exit(1);
}

await browser.close();