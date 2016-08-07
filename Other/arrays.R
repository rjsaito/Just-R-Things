#use of arrays for 2+ dimensional categories

VER <- "RA.RL15_no.OF"    #1/15/2015 UPDATED
outVER = "_RA.RL15_no.OF_reduced"  #1/15/2015 UPDATED
outstr = array(c(outer(c("","_w.MUK","_w.mobs","_w.MUK_w.mobs","_w.HDB","_w.MUK_w.HDB",
                         "_w.HDB_w.mobs","_w.MUK_w.HDB_w.mobs"),c("","_RA"),FUN=paste, sep="")),dim = c(2,2,2,2))
outversion = outstr[(MUKWONGA+1),(include.mobs+1),(HDB+1),(RA+1)]
bootver = outstr[(MUKWONGA+1),1,(HDB+1),1]
segver = outstr[(MUKWONGA+1),1,1,1]