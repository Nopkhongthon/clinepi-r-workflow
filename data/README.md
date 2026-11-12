# Data

`fbs_mockup_trial.xlsx` is a mock-up made for teaching. No real patient is involved.

The workbook has two sheets. The `data` sheet holds the data. The `about` sheet repeats the notes below.

## Layout of the data sheet

The sheet has one row per patient. These are the columns.

- ID, Group, Age, Sex, BMI and DM duration (yr)
- a Date column and an FBS column for each of the visits M0, M3, M6, M9 and M12

FBS is fasting blood sugar. Drug A and drug B are compared.

## Kinds of dirt to clean

The sheet is typed the way several staff would type it. It holds these kinds of dirt.

- mixed date formats, including Buddhist Era years, Thai month names and Excel serial numbers
- many spellings for the same group and the same sex
- patient numbers typed with a prefix or leading zeros
- units typed inside cells, such as years or kg/m2 or mg/dL
- glucose in mmol/L typed into the mg/dL column
- missing-value codes, such as 999 or 99, and text such as N/A or a Thai phrase for did not come
- a mistyped year
- duplicate rows

Chapter 1 of the page cleans each of these in turn. The script is `code/1_clean_data.r`.
