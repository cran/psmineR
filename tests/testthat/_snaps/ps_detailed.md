# test ps_detailed on eventlog fails when 'segment_coverage' != [0,1]

    `segment_coverage` must be a <numeric> between 0 and 1.
    x You supplied a <numeric>: -0.1

---

    `segment_coverage` must be a <numeric> between 0 and 1.
    x You supplied a <numeric>: -0.1

---

    `segment_coverage` must be a <numeric> between 0 and 1.
    x You supplied a <numeric>: 2

---

    `segment_coverage` must be a <numeric> between 0 and 1.
    x You supplied a <character>: "0.5"

# test ps_detailed on eventlog fails when 'n_segments' < 0 or not an integer

    `n_segments` must be an interger-like <numeric> larger than 0.
    x You supplied a <numeric>: -1

---

    `n_segments` must be an interger-like <numeric> larger than 0.
    x You supplied a <numeric>: 2.5

---

    `n_segments` must be an interger-like <numeric> larger than 0.
    x You supplied a <character>: "5"

# test ps_detailed on eventlog fails when both 'segment_coverage' and 'n_segments' are provided

    Must supply `segment_coverage` or `n_segments`, but not both.

# test ps_detailed on eventlog fails on invalid classification

    Invalid `classification`.
    x "var" is not present in log.

