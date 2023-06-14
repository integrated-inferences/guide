
library("pacman")

pacman::p_load(
  tidyverse,
  gbiqq,
  gbiqqtools,
  bindrcpp,
  ggplot2,
  StanHeaders,
  rstan,
  knitr,
  expm,
  dagitty,
  stargazer,
  partitions)

do_diagnosis = FALSE

if(do_diagnosis) {if(!exists("fit")) fit <- fitted_model()}

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

format_with_col = function(x, color = "blue"){
  if(knitr::is_latex_output())
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(knitr::is_html_output())
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}



#' Game Tree
#'
#' Thus function is in development. It draws game trees and identifies subgame perfect equilibria when this is unique
#' Backwards induction is implemented under the assumption that utilities are "generic" and so a unique solution exits.
#'
#' In this version the graphing is not good when there are multiple solutions or indifference at given nodes.
#'
#' @param H: A matrix of histories with one row per terminal node, and one column per time period, entries are node labels. First column is normally all 0s for the origin; if first column has variation an origin is added to the hostory.
#' @param U: A matrix of utilities with one row per terminal node, and one column per player
#' @param P: A matrix of players identifiers (numbered according to columns in U), row per terminal node, and one column per time period (less one)
#' @param textsize Text size
#' @param solution FALSE to turn off computation of SPNE
#' @param force_solution TRUE to force a solution for a non-generic game
#' @param offset Used to adjust  text offsetting
#' @param player.names, a matrix of player labels in the order specified in U
#' @param thickness of line for SPNE
#' @param sadj does a fix on the text slope
#' @param uss indicates to write utilities in form u_name; otherwise just name
#' @param utextsize is textsize for utilities
#' @keywords Game tree
#' @export
#' @examples
#' # A chicken game:
#'   H <- matrix(c("C", "C", "D", "D", "C", "D",  "C", "D"),4)
#'   U <- matrix(c(1,0,2,-1,1,2, 0, -1), 4)
#'   P <- matrix(c(rep(1,4), rep(2,4)),4)
#'   gt_tree(H,U,P, title = "Sequential Chicken")
#' # A game in which options depend on past history.
#'   H <- matrix(c("Take hostages", "Take hostages", "             Don't take hostages", "Pay Ransom", "Don't pay", ""),3)
#'   U <- matrix(c(1,-1,0,-1,-2,0), 3)
#'   P <-  matrix(c(rep(1,3), c(2,2,1)),3)
#'   gt_tree(H,U,P,solution=FALSE, player.names=c("Militants", "Gov"))
#' Same game with a solution, though note the phantom highlighting in this example.
#'   gt_tree(H,U,P,solution=TRUE, player.names=c("Militants", "Gov"))

gt_tree = function(
  H,
  U,
  P,
  player.names=c(1:ncol(U)),
  playercol = "black",
  title="",
  titlecol="black",
  textsize=1.5,
  titlesize=textsize,
  utextsize= textsize,
  btextsize= textsize,
  solution=TRUE,
  warnings = TRUE,
  force_solution = FALSE, # Attempt solution even for non-generic games
  mark.branches=((ncol(H)-1):1),    # indicate whether to not mark solutions for some branches; eg exclude first decision if made by nature: mark.branches=((ncol(H)-1):2)
  offset=.18,
  thickline=3,
  print.utilities = rep(TRUE, ncol(U)),  # Indicate which utilities get printed
  uspacing=.5,
  slopetext = TRUE,
  actioncol="blue",
  branchcol="black",
  solutioncol = "black",
  sadj=ncol(H)/(nrow(H)+1),
  uss=TRUE,
  angled = FALSE  # ANGLED MAKES SECOND LAST BRANCH ANGLED FOR CLARITY; NOT GERNALLY NEEDED
){
  if(!is.matrix(H)) stop("H should be a matrix")
  if(!is.matrix(U)) stop("U should be a matrix")
  if(!is.matrix(P)) stop("P should be a matrix")
  if(length(unique(H[,1]))!=1) {H <- cbind(rep(0, nrow(H)), H); print("Initial history column added")}
  N  <- nrow(H)
  n  <- ncol(U)
  if(solution &  ( length(apply(U, 2, unique)) !=(n*N))) {
    if(force_solution & warnings) print("Warning: Solution attempted even though game is not generic")
    if(!force_solution) {solution <- FALSE; print("Game is not generic and solution not attempted.")}
  }
  C  <- 1:N
  t  <- ncol(H)
  P2 <- cbind(rep("X", N), P)
  H2 <- H
  H2[,2:t][(H[,2:(t)]==H[,1:(t-1)])  & (P2[,2:(t)]==P2[,1:(t-1)])] <- ""
  STRAT_SET 	<- function(i,j){sapply(1:N, function(k) sum(H[i,1:j]==H[k,1:j])==j)}
  POINTS 			<- POINTS2 <- sapply(1:t, function(j) sapply(1:N, function(i)   mean(C[STRAT_SET(i,j)])))
  UNIQUE 			<- sapply(1:t, function(j) sapply(1:N, function(i)   length(C[STRAT_SET(i,j)])))==1
  POINTS2[UNIQUE]	<- NA												# USED TO ENSURE THAT POINTS/LABELS ARE NOT DRAWN IN THE MIDDLE OF LONG BRANCHES
  POINTS2[,t] <- 1:N

  if(angled)	POINTS2[,t-1][is.na(POINTS2[,t-1])] <- POINTS2[,t][is.na(POINTS2[,t-1])]
  POINTS3 <- POINTS2
  for(j in 1:N){POINTS2[j,] <-	approx(POINTS2[j,], n = t)$y}
  par(mar=c(.5, .5,.5, .2))
  plot(c(.75,t+n*uspacing+.25), c(.75,N+.5), col=0, xlab="", ylab="", axes=F)			    # PLOT AREA
  for(i in 1:(t-1)){segments(i, POINTS2[,i], i+1, POINTS2[,i+1], col = branchcol)}		# DRAW TREE

  if(solution){													# IDENTIFY SPNE IF REQUIRED
    SE <- BR <- cbind(matrix(NA, N, t-1), c(1:N)) 					# INITIALIZE STRATEGIC EQUIVALENT AND BEST RESPONSE MATRICES
    best  <- function(i,j) mean(sapply(C[STRAT_SET(i,j)],
                                       function(a) C[STRAT_SET(i,j)][U[SE[STRAT_SET(SE[a , j+1],j),j+1], P[a,j]] ==
                                                                       max(U[SE[STRAT_SET(SE[a , j+1],j),j+1], P[a,j]])]
    ))
    for( j in mark.branches){  # Note mark.branches goes in reverse order to implement backward induction
      SE[,j] <- sapply(1:N, function(i) SE[max(best(i,j)),j+1])
      BR[,j] <- sapply(1:N, function(i) mean(best(i,j)))
    }

    for(j in mark.branches)for(i in 1:N){
      # Draw lines for histories
      if(!UNIQUE[i,j]){ # Added to remove ghost limbs where a person has already determined the move
        segments(j,
                 POINTS2[i,j],
                 #j + sum(sapply(j:(t-1), function(h) BR[i,h]==BR[i,j])),
                 min(t, j+ max(apply(BR[, j:t] == BR[i,j], 1, sum))), # horizontal shift is hard part - may not work with complex graphs; this approach
                 # takes shift to rhe furthest horizontal point of best responses involving the best response hisory
                 BR[i,j],
                 lwd=thickline, col = solutioncol)
      }
    }
  }

  # LABEL BRANCHES
  if(slopetext) for(i in 1:(t-1))for(j in 1:(N)){text(i+.5, (.5*POINTS2[j,i]+.5*POINTS2[j,i+1] + offset*sign(POINTS2[j,i+1] - POINTS2[j,i])), H2[j,i+1], cex=btextsize, col=actioncol, srt = atan2((POINTS2[j,i+1]-POINTS2[j,i])*sadj,1)*180/pi)}		# LABEL STRATEGIES
  if(!slopetext) for(i in 1:(t-1)){text(i+.5, (.5*POINTS2[,i]+.5*POINTS2[,i+1] + offset*sign(POINTS2[,i+1] - POINTS2[,i])), H2[,i+1], cex=btextsize, col=actioncol)}		# LABEL STRATEGIES

  # MARK UTILITIES
  n2 <- (sum(print.utilities))
  for(i in 1:n2){text(rep(t+i*uspacing,N), 1:N, U[,print.utilities][,i],  cex=textsize)}
  if(uss)  for(i in 1:n2){text(t+i*uspacing, N+.5, bquote(u[.(player.names[print.utilities][i])]),cex=utextsize)}	# HEADER FOR UTILITIES
  if(!uss) for(i in 1:n2){text(t+i*uspacing, N+.5, player.names[print.utilities][i],cex=utextsize)}	# HEADER FOR UTILITIES

  # ADD PLAYER LABELS TO NODES
  for(i in 1:(t-1)){for(j in 1:N){text(i, POINTS3[j, i]+offset, player.names[P[j, i]], cex=textsize, col = playercol)} }

  for(i in 1:(t-1)){points(rep(i,N), POINTS3[,i], pch=19)}			# ADD LATE NODES
  points(1, POINTS2[1,1], pch=21, col="black", bg="white", cex=1.2)	# ADD FIRST NODE

  # ADD TITLE
  text(1, N+.5, title,  cex=titlesize, pos = 4, col = titlecol )
}


# Draw DAG

hj_dag <- function(x,
                   y,
                   names,
                   arcs = cbind(0,0),
                   add_points = FALSE,
                   solids = rep(1, length(x)),
                   title = "",
                   contraction = .1,
                   add_functions = 0,
                   add_functions_text = NULL,
                   text_shift = .2*add_points,
                   padding = .5,
                   length = 0.2,
                   cex = 1,
                   box = TRUE) {
  if(add_points)  plot(x, y, pch=ifelse(solids == 1, 19, 1), cex = 2, axes = FALSE, xlab = "", ylab = "",
                       xlim = c(min(x)-padding, max(x)+padding),
                       ylim = c(min(y)-padding-add_functions, max(y)+padding),
                       main = title)
  if(!add_points)  plot(x, y, type = "n", cex = 2, axes = FALSE, xlab = "", ylab = "",
                        xlim = c(min(x)-padding, max(x)+padding),
                        ylim = c(min(y)-padding-add_functions, max(y)+padding),
                        main = title)

  arrows(x[arcs[,1]]*(1-contraction) + x[arcs[,2]]*contraction,
         y[arcs[,1]]*(1-contraction) + y[arcs[,2]]*contraction,
         x[arcs[,2]]*(1-contraction) + x[arcs[,1]]*contraction,
         y[arcs[,2]]*(1-contraction) + y[arcs[,1]]*contraction, length = length)
  text(x, y + text_shift, names, cex = cex)
  if(!is.null(add_functions_text)) text(((min(x)+max(x))/2), min(y)-1, add_functions_text)
  if(box) box()
}



perm_bb <- function(v) {
  sapply(1:length(v), function(x) {
    rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
         length.out=prod(v))
  } ) - 1
}
