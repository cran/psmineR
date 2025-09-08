## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8
)

## ----setup--------------------------------------------------------------------
library(psmineR)
library(bupaR)
library(edeaR)

## -----------------------------------------------------------------------------
traffic_fines %>%
	ps_detailed()

## -----------------------------------------------------------------------------
traffic_fines %>%
	ps_detailed(n_segments = 10)

## -----------------------------------------------------------------------------
traffic_fines %>%
	ps_detailed(classification = "resource")

## -----------------------------------------------------------------------------
traffic_fines %>%
	end_activities("case") %>%
	augment(traffic_fines, prefix = "end") %>%
	ps_detailed(classification = "end_activity")

## -----------------------------------------------------------------------------
traffic_fines %>%
	end_activities("case") %>%
	augment(traffic_fines, prefix = "end") %>%
	group_by(end_activity) %>%
	ps_detailed()

## -----------------------------------------------------------------------------
traffic_fines %>%
	end_activities("case") %>%
	augment(traffic_fines, prefix = "end") %>%
	group_by(end_activity) %>%
	ps_aggregated()

