# -*- coding: utf-8 -*-
"""Build a realistically dirty Excel version of the simulated FBS trial.

Input : data/fbs_sim_wide.csv   (written by code/0_simulate_data.r, the clean truth)
Output: data/fbs_sim_dirty.xlsx (the teaching input for code/1_clean_data.r)

The layout is the one a clinic types: one row per patient, a date and an FBS
column for every visit. The dirt is the kind several data-entry staff produce
over two years: mixed date formats incl. Buddhist Era and Thai month names,
runaway category spellings, mmol/L typed into a mg/dL column, units typed into
cells, missing-value sentinels, a mistyped year and duplicate rows.

Run: python3 data/_source/make_dirty_fbs.py   (from any folder, needs openpyxl only)
"""

import csv
import random
import datetime as dt
from pathlib import Path

from openpyxl import Workbook
from openpyxl.styles import Font

SEED = 20261112
random.seed(SEED)

# paths come from this file's own folder, so it runs from any working directory
HERE = Path(__file__).resolve().parent
SRC = HERE.parent / "fbs_sim_wide.csv"
OUT = HERE.parent / "fbs_sim_dirty.xlsx"

MONTHS = [0, 3, 6, 9, 12]

THAI_MONTH = ["ม.ค.", "ก.พ.", "มี.ค.", "เม.ย.", "พ.ค.", "มิ.ย.",
              "ก.ค.", "ส.ค.", "ก.ย.", "ต.ค.", "พ.ย.", "ธ.ค."]

# each row was typed by one person, and each person had one date habit
STYLES = ["dmy"] * 40 + ["iso"] * 20 + ["excel"] * 15 + ["be"] * 15 + ["thai"] * 10

ARM_A = ["A", "a", "Drug A", "drug A", "ยา A", "group A"]
ARM_B = ["B", "b", "Drug B", "drug B", "ยา B", "group B"]
SEX_MALE = ["1", "M", "m", "Male", "male", "ชาย", "MALE"]
SEX_FEMALE = ["0", "F", "f", "Female", "female", "หญิง", "FEMALE"]
NA_TOKENS = ["", ".", "-", "N/A", "NA", "na"]
MISSED_VISIT = ["", "", "", "-", "ไม่มา", "N/A"]   # ไม่มา = did not come


def fmt_date(d, style):
    """Render a date the way a particular data-entry habit would."""
    if d is None:
        return None
    if style == "dmy":
        return f"{d.day:02d}/{d.month:02d}/{d.year}"
    if style == "iso":
        return d.isoformat()
    if style == "excel":
        return dt.datetime(d.year, d.month, d.day)   # a true Excel date cell
    if style == "be":
        return f"{d.day:02d}/{d.month:02d}/{d.year + 543}"
    if style == "thai":
        return f"{d.day} {THAI_MONTH[d.month - 1]} {d.year + 543}"
    raise ValueError(style)


def pad(x):
    """Stray spaces, some of the time."""
    r = random.random()
    if r < 0.10:
        return f" {x} "
    if r < 0.18:
        return f"{x} "
    return x


def main():
    with open(SRC, encoding="utf-8") as f:
        src = list(csv.DictReader(f))
    n = len(src)
    idx = list(range(n))

    styles = STYLES[:]
    random.shuffle(styles)
    styles = (styles * (n // len(styles) + 1))[:n]

    # rows chosen up front so the dirt is reproducible
    prefix_id = set(random.sample(idx, 30))                    # "FBS-023"
    zero_pad_id = set(random.sample(sorted(set(idx) - prefix_id), 20))  # "023"
    age_text = set(random.sample(idx, 4))                      # "62 ปี"
    age_blank = set(random.sample(sorted(set(idx) - age_text), 2))
    age_sentinel, age_impossible = random.sample(sorted(set(idx) - age_text - age_blank), 2)
    sex_blank = set(random.sample(idx, 2))
    bmi_slip = set(random.sample(idx, 2))                      # 27.3 -> 273
    bmi_unit = set(random.sample(sorted(set(idx) - bmi_slip), 4))  # "27.3 kg/m2"
    dm_text = set(random.sample(idx, 5))                       # "5 ปี" / "5 yr"
    dm_sentinel = random.choice(sorted(set(idx) - dm_text))    # 99

    # visit-level dirt, as (row, month) cells that were really measured
    measured = [(i, m) for i in idx for m in MONTHS if src[i][f"fbs_m{m}"] != "NA"]
    cells = random.sample(measured, 26)
    fbs_mmol = set(cells[:12])        # typed in mmol/L, the glucometer unit
    fbs_unit = set(cells[12:20])      # "152 mg/dL"
    fbs_sentinel = set(cells[20:21])  # 999
    year_typo = set(cells[21:22])     # 2025 -> 2115

    rows = []
    for i, r in enumerate(src):
        st = styles[i]

        raw_id = int(r["id"])
        if i in prefix_id:
            id_out = f"FBS-{raw_id:03d}"
        elif i in zero_pad_id:
            id_out = f"{raw_id:03d}"
        else:
            id_out = str(raw_id)

        arm = pad(random.choice(ARM_A if r["treatment"] == "A" else ARM_B))

        age = int(r["age"])
        if i == age_sentinel:
            age_out = 999
        elif i == age_impossible:
            age_out = age // 10           # a digit dropped: 62 -> 6
        elif i in age_text:
            age_out = f"{age} ปี"
        elif i in age_blank:
            age_out = random.choice(NA_TOKENS)
        else:
            age_out = age

        if i in sex_blank:
            sex = random.choice(NA_TOKENS)
        else:
            sex = pad(random.choice(SEX_MALE if r["sex"] == "Male" else SEX_FEMALE))

        bmi = float(r["bmi"])
        if i in bmi_slip:
            bmi_out = round(bmi * 10)
        elif i in bmi_unit:
            bmi_out = f"{bmi} kg/m2"
        else:
            bmi_out = bmi

        dm = int(r["dm_duration"])
        if i == dm_sentinel:
            dm_out = 99
        elif i in dm_text:
            dm_out = random.choice([f"{dm} ปี", f"{dm} yr", f"{dm} years"])
        else:
            dm_out = dm

        visits = []
        for m in MONTHS:
            d_raw, f_raw = r[f"visit_date_m{m}"], r[f"fbs_m{m}"]
            if f_raw == "NA":                       # dropped out: never came
                visits += [None, random.choice(MISSED_VISIT)]
                continue
            d = dt.date.fromisoformat(d_raw)
            if (i, m) in year_typo:
                d = d.replace(year=d.year + 90)
            fbs = int(float(f_raw))
            if (i, m) in fbs_mmol:
                fbs_out = round(fbs / 18, 1)
            elif (i, m) in fbs_unit:
                fbs_out = f"{fbs} mg/dL"
            elif (i, m) in fbs_sentinel:
                fbs_out = 999
            else:
                fbs_out = fbs
            visits += [fmt_date(d, st), fbs_out]

        rows.append([pad(id_out), arm, age_out, sex, bmi_out, dm_out] + visits)

    # duplicate rows: two typed twice, one typed twice with a different FBS
    dup_exact = random.sample(idx, 2)
    near_dup = random.choice([k for k in idx
                              if k not in dup_exact and isinstance(rows[k][9], int)])
    extra = [list(rows[k]) for k in dup_exact]
    nd = list(rows[near_dup])
    nd[9] = nd[9] + 5                     # FBS M3 re-entered with a typo
    extra.append(nd)

    out_rows = rows + extra
    random.shuffle(out_rows)

    header = ["ID", "Group", "Age", "Sex", "BMI", "DM duration (yr)"]
    for m in MONTHS:
        header += [f"Date M{m}", f"FBS M{m}"]

    wb = Workbook()
    ws = wb.active
    ws.title = "data"
    ws.append(header)
    for c in ws[1]:
        c.font = Font(bold=True)
    for row in out_rows:
        ws.append(row)
    for col in ws.iter_cols(min_row=2):
        for cell in col:
            if isinstance(cell.value, dt.datetime):
                cell.number_format = "dd/mm/yyyy"

    wb.save(OUT)
    print(f"wrote {OUT.name}: {len(out_rows)} rows x {len(header)} columns")
    print(f"dirty FBS cells: mmol/L {len(fbs_mmol)}, unit text {len(fbs_unit)}, "
          f"999 {len(fbs_sentinel)}, year typo {len(year_typo)}")
    print(f"exact duplicate ids {[src[k]['id'] for k in dup_exact]}, near duplicate id {src[near_dup]['id']}")


if __name__ == "__main__":
    main()
