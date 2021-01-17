pathTextGrid$ ="PATHTEXTGRID"
tiername$ = "DESTINATIONTIERNAME"
selStartSec = SELSTARTSEC
selEndSec = SELENDSEC
appendText$ = "TEXTTOINSERT"


#---get name of textgrid 
@split: pathTextGrid$, "/"
nameTextGrid$ = split.array$[split.length]
pointPos = rindex(nameTextGrid$,".")
if pointPos>1
	nameTextGrid$ = left$(nameTextGrid$, pointPos-1)
endif
appendInfoLine: "---" + nameTextGrid$

#--- try to select textgrid, otherwise open file from path
selectObject ( )
nocheck nowarn selectObject: "TextGrid " + nameTextGrid$
if numberOfSelected ( ) = 0
	if (fileReadable (pathTextGrid$)=0)
		appendInfoLine: "TextGrid not found"
		goto endOfScript
	endif
	nocheck nowarn Read from file: pathTextGrid$
endif
gridID = selected ("TextGrid", 1)


#--- try to get tier number from tier name
@tier_number_by_name (gridID, tiername$)
tierNr=result
#appendInfoLine: tierNr

#if tier was not found -> end script
if (tierNr=0)
	appendInfoLine: "Tier not found"
	goto endOfScript
endif


#--- Process start time
#get interval that includes start time
intervalNr = Get interval at time: tierNr, selStartSec

#get start time of this interval
intervalStartSec = Get starting point: tierNr, intervalNr

#insert boundary if the new time is different from the existing time
if (intervalStartSec<>selStartSec)
	Insert boundary: tierNr, selStartSec
endif


	#if the start time of this intercall is the same as new time --> 
		# take this start time/interval
		#start time is fine

	# if not 
		#check if this intervall is empty
		# if it is not, 
			#notify userthet intervall has been changed OR do not add boundary
		# if it is
			#add a boundary for start 
			#and get the new intervall number
		#if the interval before is not empty --> 


# --- process end time
#get interval that includes end time
intervalNr = Get interval at time: tierNr, selEndSec-0.00001

#get end time of this interval
intervalEndSec = Get end point: tierNr, intervalNr

#insert boundary if the new time is different from the existing time
if (intervalEndSec<>selEndSec)
	Insert boundary: tierNr, selEndSec
endif

	# get end time of current intervall
	# if this time is bigger than new end time -> add new boundary
		#if the intervall is not empty --> notify user that intervall boundaries have been changed OR do not add boundary

	# if this time is smaller than new end time --> boundary cannot be set
		#notify user that right bourder coud not be set


#---- process text
#get interval that includes start time
intervalNr = Get interval at time: tierNr, selStartSec

#get text of intervall
intervalText$ = Get label of interval: tierNr, intervalNr

if (intervalText$="")
	newText$ = appendText$
else
	newText$ = intervalText$ + " " + appendText$
endif

#set new text
Set interval text: tierNr, intervalNr, newText$
appendInfoLine: "... new text set: " + newText$

#this is the end:
label endOfScript



#------------ PROCEDURES
#--- get the tier number by its name
procedure tier_number_by_name (gridID, name$)
# Return in <result> the number of first tier corresponding to tier name.
# Return 0 if tier with name does not exist in textgrid.
# The target tier name may be a regular expression.
# Return tier name in <result2$>
   select gridID
   n_ = Get number of tiers
   result = 0
   tier_ = 1
   while (tier_ <= n_ and result = 0)
      result2$ = Get tier name... tier_
      if (index_regex (result2$, name$)) 
         result = tier_
      else
         tier_ += 1
         result2$ = ""
      endif
   endwhile
endproc

#--- split a string by a separator
procedure split (.str$, .sep$)
 	.seplen = length(.sep$) 
 	.length = 0
 	repeat
		.strlen = length(.str$)
		.sep = index(.str$, .sep$)
		if .sep > 0
      			.part$ = left$(.str$, .sep-1)
			.str$ = mid$(.str$, .sep+.seplen, .strlen)
    		else
     			.part$ = .str$
    		endif
		.length = .length+1
		.array$[.length] = .part$
  	until .sep = 0
endproc