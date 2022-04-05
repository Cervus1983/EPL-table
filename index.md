English Premier League table
================

``` r
library(tidyverse)
```

We’ll be using [English Premier League
data](https://github.com/Cervus1983/EPL-data/blob/master/index.md) we
learned to download in the previous post:

``` r
epl
```

    ## # A tibble: 10,804 x 6
    ##    Season Date     HomeTeam         AwayTeam        FTHG  FTAG
    ##     <int> <chr>    <chr>            <chr>          <dbl> <dbl>
    ##  1   1993 14/08/93 Arsenal          Coventry           0     3
    ##  2   1993 14/08/93 Aston Villa      QPR                4     1
    ##  3   1993 14/08/93 Chelsea          Blackburn          1     2
    ##  4   1993 14/08/93 Liverpool        Sheffield Weds     2     0
    ##  5   1993 14/08/93 Man City         Leeds              1     1
    ##  6   1993 14/08/93 Newcastle        Tottenham          0     1
    ##  7   1993 14/08/93 Oldham           Ipswich            0     3
    ##  8   1993 14/08/93 Sheffield United Swindon            3     1
    ##  9   1993 14/08/93 Southampton      Everton            0     2
    ## 10   1993 14/08/93 West Ham         Wimbledon          0     2
    ## # ... with 10,794 more rows

Function to generate league tables from match results:

``` r
league_table <- function(results) inner_join(
    results %>% 
        group_by(
            Team = HomeTeam
        ) %>% 
        summarise(
            Pts = sum(FTHG > FTAG) * 3 + sum(FTHG == FTAG) * 1,
            GF = sum(FTHG),
            GA = sum(FTAG),
            .groups = "drop"
        ) %>% 
        pivot_longer(
            -Team
        ),
    results %>% 
        group_by(
            Team = AwayTeam
        ) %>% 
        summarise(
            Pts = sum(FTHG < FTAG) * 3 + sum(FTHG == FTAG) * 1,
            GF = sum(FTAG),
            GA = sum(FTHG),
            .groups = "drop"
        ) %>% 
        pivot_longer(
            -Team
        ),
    by = c(
        "Team",
        "name"
    )
) %>% 
    transmute(
        Team,
        name,
        value = value.x + value.y
    ) %>% 
    pivot_wider() %>% 
    arrange(
        -Pts,
        GA - GF,
        -GF,
        rev(Team)
    ) %>% 
    mutate(
        Pos = row_number()
    ) %>% 
    select(
        Pos,
        everything()
    )
```

For example, the final standings for 2020/21:

``` r
epl %>% 
    filter(
        Season == 2020
    ) %>% 
    league_table() %>% 
    print.data.frame(
        row.names = FALSE
    )
```

    ##  Pos             Team Pts GF GA
    ##    1         Man City  86 83 32
    ##    2       Man United  74 73 44
    ##    3        Liverpool  69 68 42
    ##    4          Chelsea  67 58 36
    ##    5        Leicester  66 68 50
    ##    6         West Ham  65 62 47
    ##    7        Tottenham  62 68 45
    ##    8          Arsenal  61 55 39
    ##    9            Leeds  59 62 54
    ##   10          Everton  59 47 48
    ##   11      Aston Villa  55 55 46
    ##   12        Newcastle  45 46 62
    ##   13           Wolves  45 36 52
    ##   14   Crystal Palace  44 41 66
    ##   15      Southampton  43 47 68
    ##   16         Brighton  41 40 46
    ##   17          Burnley  39 33 55
    ##   18           Fulham  28 27 53
    ##   19        West Brom  26 35 76
    ##   20 Sheffield United  23 20 63

This can be verified against
e.g. [Wikipedia](https://en.wikipedia.org/wiki/2020%E2%80%9321_Premier_League#League_table).
