library(betr)
library(reshape2)

# TODO:
# - exper instrns/paper instrns?
# - lab test: instrns ok?
# - lab test: connection reset in quiz timed stage?
# - treatments per group
#    - one "all different", one "3-3", one "2-2-2"?
#    - all different means "no initial groups"
#    - but also very specific and easy to make inferences?
#    - random gives natural variation in e.g. "heterogeneity" or "polarization"
#    - and will encourage targeting of standout minorities (probably)
#    - but can create more variation deliberately...
# - group ID measurement?

# ===== poss future treatments =====
# changers are marked? 
# cost/elicit cost to move?

testmode <- FALSE
session <- if (exists('session')) session + 1 else 1
gs <- if (testmode) 2L else 6L # group size
N <- if (testmode) 2L else 24L # session size
endowment <- 12.00 # per round
gainloss <- 10.00
tcost <- 0.50 # cost of targeting someone
showup_fee <- 2.50
countdown <- 60 # for timer
n_change_cols <- if (testmode) 2L else 2L # number who change colours in each group each round
n_reps <- if (testmode) 2L else 20L # number of repetitions
min_targeters <- if (testmode) 2L else 2L # to successfully expropriate victim
mycolours <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33")
img_prefix <- if (testmode) "http://127.0.0.1" else ""
seed <- c(653198432L, 1324689L, 186079134L, 213468933L, 132463232L)[session]
if (N %% gs) stop("N doesn't divide into group size")
clients_in_url <- testmode
on_ready <- function() {
  globals <<- NA
  random_order <<- NA
  timeout <<- NA
  groups <<- sample(rep(1:(expt$N/gs), gs))
  mydf <<- experiment_data_frame(expt, group=groups, passed_quiz=NA,
        colour=sample(mycolours, N, replace=TRUE), can_change=NA,
        suggcolour=NA, target=NA, targeted=NA, successful=NA,
        pos=NA, profit=NA, period_chosen=NA,
        q_about=NA, q_comments=NA, q_gender=NA, q_teamsports=NA, q_othsports=NA,
        stringsAsFactors=FALSE)
}

expt <- experiment(N=N, name="gangs", on_ready=on_ready, seed=seed,
      clients_in_url=clients_in_url, client_refresh=2)

s_rules <- text_stage(b_brew("rules.brew"), name="Rules", wait=TRUE)
s_instr <- text_stage(b_brew("instr.brew"), name="Instructions", wait=TRUE)
s_quiz <- timed(stage(function(id, period, params) {
        if (isTRUE(params$passed_quiz=="1")) {
          mydf$passed_quiz[mydf$id==id] <<- 1
          return(NEXT)
        }
        return(b_brew("quiz.brew")(id, period, params, NULL))
      }, 
      name="Quiz"), 
      timeout=180, on_timeout=function(id, period) {
        mydf$passed_quiz[mydf$id==id] <<- 0 
      })

s_answers <- text_stage(b_brew("answers.brew"), name="Quiz answers")

p_carry_over <- program(run="first", function(id, period, ...) {
    if (period > 1) mydf$colour[mydf$period==period] <<- 
          mydf$colour[mydf$period==period-1]
    mydf$can_change[mydf$period==period] <<- FALSE
    mydf$can_change[mydf$period==period][sample(1:N, n_change_cols*N/gs)] <<-
          TRUE
  },
  name="Carry over colours")

really_is_colour <- is_one_of(mycolours)
is_colour <- function(ftitle, val, id, period, params) {
  if (! mydf$can_change[mydf$period==period & mydf$id==id]) return(NULL)
  really_is_colour(ftitle, val, id, period, params)
}

p_timer <- program(run="all", function(id, period) {
  timeout[id] <<- as.numeric(Sys.time()) + countdown 
}, name="Start timer")


s_pick_colour <- form_stage(b_brew("pick_colour.brew"), 
      fields=list(suggcolour=is_colour), 
      titles=list(suggcolour="Colour"),
      data_frame="mydf",
      name="Pick colours")

p_prepare <- program(run="last", function(id, period, ...){
    for (g in unique(mydf$group)) {
      gp <- mydf$period==period & mydf$group==g
      old <- mydf$colour[gp]
      sugg <- mydf$suggcolour[gp]
      newcol <- old
      tochange <- mydf$can_change[gp]
      newcol[tochange] <- sugg[tochange]
      mydf$colour[gp] <<- newcol
      mydf$pos[gp] <<- sample(1:gs)
    }
    random_order <<- sapply(1:N, function(x) {
      g <- mydf$group[mydf$id==x & mydf$period==period]
      sample(mydf$id[mydf$group==g & mydf$period==period])
    }) 
  }, name="Prepare")

is_target <- function(ftitle, val, id, ...) {
  if (!is.null(has_value()(ftitle, val, ...))) 
        return("Please choose a target")
  if (! val %in% c(0, random_order[,id])) return("Please choose a target")
  return(NULL)
}

s_pick_target <- form_stage(b_brew("pick_target.brew"),
      fields=list(target=is_target),
      titles=list(target="Target"),
      data_frame="mydf",
      name="Pick target")

p_results <- program("first", function(id, period,...) {
    # for each potential victim
    for (g in unique(mydf$group)) {
      myg <- mydf$period==period & mydf$group==g
      mydf$profit[myg] <<- endowment
      for (vic in unique(mydf$id[myg])) {
        targeters <- mydf$id[myg & mydf$target == vic]
        nt <- length(targeters)
        vid <- mydf$id==vic & mydf$period==period
        tid <- mydf$id %in% targeters & mydf$period==period
        mydf$targeted[vid] <<- nt
        if (nt >= min_targeters) {
          mydf$profit[vid] <<- mydf$profit[vid] - gainloss
          mydf$profit[tid] <<- mydf$profit[tid] + gainloss/nt
          mydf$successful[tid] <<- TRUE
        } else {
          mydf$successful[tid] <<- FALSE
        }
      }
      mydf$profit[myg] <<- ifelse(mydf$target[myg] > 0,
            pmax(mydf$profit[myg] - tcost, 0), mydf$profit[myg])
      mydf$profit[myg] <<- round(mydf$profit[myg], 2)
    }
  }, name="Results")

s_results <- text_stage(b_brew("results.brew"), name="Period results")

write_payment_data <- function() {
  globals <<- dcast(melt(mydf[,c("id", "period", "profit")], id=1:2),
    id ~ period)
  pc <- sample(n_reps, 1)
  globals$period_chosen <<- pc
  mydf$period_chosen <<- pc
  globals$totalprofit <<- mydf$profit[mydf$period==pc]
  globals$totalprofit <<- sprintf("%.2f", globals$totalprofit + showup_fee)
  globals <<- merge_subjects(expt, globals)[,c("seat", "id", "totalprofit")]
  globals <<- globals[order(globals$seat, globals$id),]
  payfile <- paste0("session-", session, "-paydata.csv")
  write.csv(globals, file=payfile, row.names=FALSE)
  cat("Payment data written to", sQuote(payfile), ".\n")
  cat("Look at 'globals' to display it now.\n")
}

p_calculate_profit <- program(run="first", function(...){
  write_payment_data()
}, name="Calculate profit")

s_final_results <- text_stage(b_brew("final_results.brew"), 
      name="Final results")
s_quaire <- form_stage(b_brew("quaire.brew"), 
      fields=list(
        q_about=has_value(), 
        q_comments=function(...) return(NULL),
        q_gender=is_one_of("male", "female"),
        q_teamsports=is_one_of("yes", "no"),
        q_othsports=is_one_of("yes", "no")
      ),
      titles=list(
        q_about="What did you think this experiment was about",
        q_comments="Comments",
        q_gender="Gender",
        q_teamsports="Team sports",
        q_othsports="Other sports"
      ),
      data_frame="mydf",
      name="Questionnaire")

p_write_data <- program(run="last", function(...) {
  write_data(expt, mydf)
  message("mydf written to csv file")
},
name="Write experiment data")

add_stage(expt, s_rules, s_instr, s_quiz, checkpoint(), s_answers)
add_stage(expt, 
      period("all"), p_carry_over, p_timer, s_pick_colour, p_prepare, 
      checkpoint(), p_timer, s_pick_target, checkpoint(), p_results, 
      p_timer, s_results, 
      times=n_reps)
add_stage(expt, period("all"), p_calculate_profit, s_final_results,
      s_quaire, p_write_data)
load_commands(expt)

restart <- function() {
  halt(expt)
  source('gangs.R')
  ready(expt)
}
