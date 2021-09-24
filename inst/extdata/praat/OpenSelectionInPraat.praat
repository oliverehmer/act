pathTextGrid$ ="PATHTEXTGRID"
pathLongSound$ = "PATHLONGSOUND"
selStartSec = SELSTARTSEC
selEndSec = SELENDSEC
playSelection = PLAYSELECTION
closeAfterPlaying = CLOSEAFTERPLAYING

#--- get name of textgrid
@split: pathTextGrid$, "/"
nameTextGrid$ = split.array$[split.length]
pointPos = rindex(nameTextGrid$,".")
if pointPos>1
	nameTextGrid$ = left$(nameTextGrid$, pointPos-1)
endif

if pathLongSound$<>""
	#--- get name of sound
	@split: pathLongSound$, "/"
	nameLongSound$ = split.array$[split.length]
	pointPos = rindex(nameLongSound$,".")
	if pointPos>1
		nameLongSound$ = left$(nameLongSound$, pointPos-1)
	endif
endif

#--- try to select textgrid, otherwise open file from path

nocheck nowarn editor:	"TextGrid " + nameTextGrid$
test = nocheck nowarn Get cursor

if test==undefined
	#clear selection
	
	selectObject ( )
	#try to select textgrid (already in list)
	nocheck nowarn selectObject: "TextGrid " + nameTextGrid$
	numberOfSelectedObjects = numberOfSelected ()
	
	#if not found, try to load
	if numberOfSelectedObjects = 0
		Read from file: pathTextGrid$
	endif

	#--- try to select sound, otherwise open file from path
	selectObject ( )
	nocheck nowarn selectObject: "LongSound " + nameLongSound$
	numberOfSelectedObjects = numberOfSelected ()
	if numberOfSelectedObjects = 0
		if pathLongSound$<>""
			nocheck nowarn Open long sound file: pathLongSound$
		endif
	endif

	#select together (if possible)
	if nameTextGrid$<>""
		selectObject: "TextGrid " + nameTextGrid$
		if pathLongSound$<>""
			nocheck nowarn plusObject: "LongSound " + nameLongSound$
		endif
	else
		if pathLongSound$<>""
			selectObject: "LongSound " + nameLongSound$
		endif
	endif
		
	Edit
endif


nowarn editor: "TextGrid " + nameTextGrid$
nowarn Select: selStartSec, selEndSec
nowarn Zoom: selStartSec-0.5, selEndSec+0.5

if playSelection==1
	Play: selStartSec, selEndSec
	if closeAfterPlaying==1
		endeditor
	endif
endif




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


