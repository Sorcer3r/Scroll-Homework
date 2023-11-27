//
//  Sorcie's Homework
//

// to do
// single line text lower down . with colour bars?
// add sprite 0 . OSK logo?  circling 
// forget colour bars on big scroller 


.label charBase = $3000
.label sinBase = $3400
.label spriteBase = $3800

.label bounce_off = 128
.label bounce_on = 129
.label setColour  = 192
.label oskSpriteOff = 224
.label oskSpriteOn = 225
.label extraTextOff = 226
.label extraTextOn = 227
.label rotateSpeed = 5

.label bottomScrollerStart = 233
.label colourListLength = colourListEnd - colourList

* = $02 "zeropage" virtual
bounceOn: .byte 0
sinPosition: .byte 0
nextchar: .byte 0
spriteColour: .byte 0
colourBarPointer: .byte 0
oskSpriteEnable: .byte 0
extraTextEnable: .byte 0
sprite0XPointer: .byte 0
sprite0YPointer: .byte 0
sprite0Frame: .byte 0
sprite0Counter: .byte 0 
scrolXSave: .byte 0 
bottomTextPos: .byte 0
bottomTextXoffset: .byte 0
* = $0801

BasicUpstart2(Start)

Start: 

        sei
		//Stop CIA interrupts
		lda #$7f
		sta $dc0d
		sta $dd0d

		//Bank out BASIC + Kernal
		lda #$35
		sta $01
        lda #$17
        sta $d018

//set up irq

        lda #<irq
        sta $fffe
        lda #>irq
        sta $ffff

        //lda #1
        //sta $d019		//ack any pending INT
        //sta $d01a		//enable raster int

        //lda #bottomScrollerStart
        //sta $d012                   //raster line to interrupt
        
        
        lda $d016               //scrolX
        sta scrolXSave

        lda #<bottomText		//setup pointer to scroll text
        sta bottomTextAdd
        lda #>bottomText
        sta bottomTextAdd+1




		// Set Fore/back colors and clear screen to black 
        // we can also set up some inital variables along the way while we have
        // the correct number in a
		lda #$00
        sta bounceOn
        sta sinPosition
        sta colourBarPointer
        sta oskSpriteEnable
        sta extraTextEnable
        sta bottomTextPos
        sta $d020
        sta $d021
        lda #1              
        sta spriteColour    //start sprite colour as white (really background since sprites are transparent)

        lda #<scrolltext
        sta scrolltextPointer
        lda #>scrolltext
        sta scrolltextPointer+1
		jsr ClearScreen

        ldx #7
        stx bottomTextXoffset
        stx nextchar    // while we have 7 may as well initialise the scroller
        ldy #SpriteStart/64 + 7
    setSpriteBase:
        tya   
        sta $7f8,x      // set sprite base
        lda #0
        sta $d027,x     // set sprite colour to black
        dey
        dex
        bpl setSpriteBase

        lda #spr_img0/64    //set up sprite0
        sta $7f8
        lda #7
        sta sprite0Counter
        lda #0
        sta sprite0Frame
        sta sprite0XPointer
        tax
        lda sinBase,x
        clc
        adc #32
        sta $d000
        lda #64
        sta sprite0YPointer
        tax
        lda sinBase,x
        sta $d001
        lda #1
        sta $d027   //set spr0 colour to white
        sta $d01c   //spr0 is multicolor, rest are not
        lda #RED
        sta $d025   // multicolour 1
        lda #GREEN   
        sta $d026   //multicolour 2

        


        lda #$fF
        sta $d01d       //set X double width 
        lda #1
        sta $d017       // set Y to normal height 1-7, 0 is dbl height
        ldx #2          //start at sprite 1
        lda #24         //sprite 1 at x = 24, rest follow 48 pixels apart. sprite 0 not used
    setSpriteXY:
        sta $d000,x     //set sprite X
        tay             // save a
        lda sinBase     // sprite Y start at row 128 (see sin table)
        sta $d001,x     //set sprite Y
        tya             //get our save X position back
        clc
        adc #48        // move sprite X along 48 pixels (double width)
        inx
        inx             // move to next sprite
        cpx #$10        // have we done all 8?
        bne setSpriteXY // not yet
        lda #%11000000  // sprite 6 & 7 need bit 9 setting because they are on rigth side of screen (X > 255)
        sta $d010

        lda #$fe        //sprites 1-7
        sta $d015       //turn on sprites 1 - 7
        jsr PutCharInSprite7    //put first char of our text into sprite 7

        ldx #120
        lda #$a0
    nextText:        
        sta $0400+(22*40),x     //fill bottom 3 lines with inverse space
        dex
        bpl nextText

forever:
eof:
    lda $d011
    bpl eof   // wait to below line 256
    jsr bounceIt        // do the bouncy thing if enabled
    jsr scrollSprites   // move everything left a bit - takes 18 lines!
topbit:
    lda $d011   //wait until we get back to top part of screen
    bmi topbit
    lda oskSpriteEnable        
    beq doSpriteColour
    jsr sprite0Action
doSpriteColour:
    jsr SetSpriteColour
CheckExtraText:
    lda extraTextEnable
    beq allDoneHere
    lda #0
    sta extraTextEnable //turn flag off - we dont need it anymore
    lda #1
    sta $d019		//ack any pending INT
    sta $d01a		//enable raster int
    lda #$1b
    sta $d011
    lda #bottomScrollerStart
    sta $d012                   //raster line to interrupt
    cli                  //turn interrupts on should start scroller
allDoneHere:
    jmp forever         //keep doing it again and again until user gets bored and closes vice64

SetSpriteColour:
        ldx sinPosition
        lda sinBase,x       // find which line our sprites start on
    waitForSpriteTop:   
        cmp $d012           // and wait until we get there
        bcs waitForSpriteTop
        tax                 //remember which line we started on
        lda spriteColour
        sta $d021           //turn background colour on (sprites are transparent)
        txa                 //get start line back
        clc
        adc #15             // not realy bottom of sprite but we only use 8 lines so turn colour back off before the bottom
    waitForSpriteBottom:
        cmp $d012
        bcs waitForSpriteBottom 
        lda #0
        sta $d021           //turn background colour off


bounceIt:
    lda bounceOn
    beq exit
    inc sinPosition
    ldx sinPosition

    lda sinBase,x
    ldx #2 
bounce1:  
    sta $d001,x     //set sprite Y
    inx
    inx             // move to next sprite
    cpx #$10        // have we done all 8?
    bne bounce1     // not yet ^
exit:
    rts

PutCharInSprite7:

    lda #<charBase    //start at char 0
    sta charData 
    lda #>charBase
    sta charData + 1
getCharacter:
    lda scrolltextPointer: $C0DE
    tay                     // save the characterr
    inc scrolltextPointer   //and move pointer to next character in the text
    bne notPageBoundary
    inc scrolltextPointer+1
notPageBoundary:
    tya                     //restore character to a
    bne testforspecial1     //are we at the end of the text
    lda #<scrolltext        //yep lets go back to the beginning
    sta scrolltextPointer
    lda #>scrolltext
    sta scrolltextPointer+1
    jmp getCharacter        //and get the next(first!) character
testforspecial1:
    cmp #128
    bcc notspecial          //bit 7 not set so its just a character
    cmp #192                //test for bounce control    
    bcs testforspecial2 
    and #1
    sta bounceOn
    jmp getCharacter
testforspecial2:
    cmp #224                //test for change colour
    bcs oskSpriteTest
    and #$3f
    sta spriteColour
    jmp getCharacter
oskSpriteTest:                 //test for OSK sprite or colourbar line
    and #3
    cmp #2
    bcs extraTextTest
    and #1
    sta oskSpriteEnable
    lda $d015          //and set sprite enable bit
    and #$fe
    ora oskSpriteEnable
    sta $d015
    jmp getCharacter

extraTextTest:
    and #1
    sta extraTextEnable
    jmp getCharacter

notspecial:
    // work out where in char set it is
    // base + character * 8
    and #%11100000  //get bits 5/6/7  to work out which page of font data we are on
    clc
    rol     
    rol     
    rol
    rol
    adc charData + 1
    sta charData + 1
    tya                 // get original number back
    and #%00011111      // mask off bits 5/6/7 - we already dealt with them
    asl
    asl
    asl                 // * 8 for offset from start of page to this characters data
    clc
    adc charData
    sta charData        // char data now points to the 8 bytes of this char

    // sprite 7 data is at spritebase + 64 * 7
    // put data in column 3 of sprite rows 4-11  (right hand side of sprite)
    // 3 bytes per line so +9 to point to row 4 . + 2 to point to column 3
    lda #<spriteBase + (64*7) + 9 + 2
    sta sprite7char 
    lda #>spriteBase + (64*7) + 9 + 2
    sta sprite7char + 1
    
    ldx #7
    ldy #24
copy8:
    lda  charData: $C0DE,x
    eor #$ff
    sta  sprite7char: $C0DE,y
    dey
    dey
    dey
    dex
    bpl copy8
    rts


scrollSprites:

    //generate a whole bunch of rotate commands so we can shift bits left in the sprite data
    //can probably do this shorter but we have loads of RAM and this is quickest
    //we are using sprites 1-7 min double width 
    //screen visible is 320 pixels
    //double width sprites are 48 pixels
    //7 double width sprites is 336 pixels
    //so we have 16 pixels over the right side to put data into thats not visible  
    sec
    .for (var j=4; j<12; j++)
    {
        .for (var i=8; i>1; i--)
        {
            .for (var k=3; k>0; k--)
            {
                rol spriteBase + (64*(i-1)) + (j*3) + (k-1)
            }
        }
    }
    dec nextchar    //count the pixels
    bpl exitRotate        //not moved 8 yet so get out of here
    lda #7          //reset pixel counter
    sta nextchar
    jsr PutCharInSprite7    //put next character into sprite7
exitRotate:    
    rts

        
ClearScreen: 
		lda #$20
		ldx #0
	loop:
		sta $0400, x
		sta $0500, x
		sta $0600, x
		sta $0700, x

		sta $d800, x
		sta $d900, x
		sta $da00, x
		sta $db00, x
		inx
		bne loop
		rts


sprite0Action:
    dec sprite0Counter
    bne sprite0_2
    lda #rotateSpeed
    sta sprite0Counter
sprite0_1:
    lda sprite0Frame
    clc
    adc #1
    and #7
    sta sprite0Frame
    clc
    adc #$e8
    sta $7f8
sprite0_2:
    dec sprite0XPointer
    ldx sprite0XPointer
    lda sinBase,x
    clc
    adc#32
    sta $d000
    dec sprite0YPointer
    ldx sprite0YPointer
    lda sinBase,x
    sta $d001
sprite0exit:   
    rts


irq:
    pha
    txa
    pha
    tya
    pha

    lda #bottomScrollerStart
irq1:
	cmp $d012
	bne irq1
	pha
	pla
	pha
	pla
	pha
	pla
	pha
	pla	
    pha
	pla
	pha
	pla

	lda $d016       	//scrolX
	and #$07            // turn on 38col
	ora bottomTextXoffset		// set scrollx offset
	sta $d016
		
		
	lda #bottomScrollerStart
	//sta currentRaster
	sta $d012
		
	//	asl vic.vicirq			//clear int flag
	
//colour bars
	
	ldx #0
Colours1:
	lda colourList,x
	tay
	lda $d012
Colours2:
	cmp $d012		//changed line yet?
	beq Colours2
	sty $d021       //background colour
	inx
	lda #bottomScrollerStart+9
	cmp $d012
	bne Colours1        //repeat accross the rasters of the text
	
colours3:
	cmp $d012
	beq  colours3
	
	lda #0
	sta $d021

    lda #bottomScrollerStart     //reset raster interrupt line
	sta $d012

	lda scrolXSave
	sta $d016   		// restore scrolx (back out of 38CL mode

	ldx bottomTextXoffset
    dex
	txa
	and #%00000111
	sta bottomTextXoffset

	cmp #7
	bne dontScroll				// if textscrollX != 7 we havent got next char yet

	jsr ScrollBottomText
dontScroll:
    lda bottomTextXoffset
    and #1
    bne dontRotate
    jsr rotateColours
dontRotate:


	asl $d019 //clear int req
    pla
    tay
    pla
    tax
    pla
    rti
//end irq

ScrollBottomText:

	ldy #1           // 2nd char on line
ShiftLeft:
	lda $0400+(23*40),y
	sta $0400+(23*40)-1,y
	iny
	cpy #40
	bne ShiftLeft
			
getNextCharacter:	
	lda bottomTextAdd: bottomText
	bne notZero		    //not 0 terminator
	lda #<bottomText		//setup opointer to scroll text
	sta bottomTextAdd
	lda #>bottomText
	sta bottomTextAdd+1
	jmp getNextCharacter
notZero:
	ora #$80
	sta $0400+(23*40)+39
	inc bottomTextAdd
	bne exitScroller
	inc bottomTextAdd+1
exitScroller:
    rts

rotateColours:
	lda colourList
	tay         // save 1st colour
	ldx #0
rotate1:
	lda colourList+1,x
	sta colourList,x
	inx
	cpx #colourListLength
	bne rotate1
	tya
	sta colourList,x
    rts


scrolltext:     // chars above 127 trigger effects on /off  128/9 = bounce off/on, 192 + x = colour, 224/5 = Sprite 0 on /off 226/7 hidden line with colour bars on/off
        .text "OldSkoolCoder gave us homework. A text scroller he said... "
        .byte bounce_on
        .text " We thought it would be fun to add extra things."
        .byte bounce_off
        .text" We can turn bounce on and off."
        .byte bounce_on
        .text " We can change colour. Lets try"
        .byte setColour + RED
        .text " RED(Really? you call this RED), or maybe we want"
        .byte setColour + YELLOW
        .text " YELLOW. I've been told some people like the colour"
        .byte setColour + PURPLE
        .text " Purple? "
        .text "oh wait! I found a spare sprite."
        .byte oskSpriteOn
        .text " No Comments on my graphics skillz please!"
        .byte setColour + GREEN 
        .text "  The only thing left to do is"
        .byte extraTextOn
        .text " colour bars. I'm rubbish at colour sequences so you'll have to make do with this."
        .byte bounce_off
        .text "         BYE!!!                      "
        .byte setColour + WHITE
        .byte oskSpriteOff
        .byte 0 // text terminator

bottomText:
    .text " OSK did fancy colours on his scroller so I suppose I should do the same. While we are here...  GREETZ to all the OSK"
    .text " crew:  Garymeg, SP175, Docster, Mikroman, Waulok, 6502Kebab and any others who I can't think of at the moment. Blame old age :("
    .text "                        "
    .byte 0

* = charBase "Charset"
CharSet:
.import binary "464charset-00-7F.bin"   // charset at $3000 - $33ff

* = $3400 "SinTable"  // sin data starting at line 100  $3400-$34ff
SinTable: 
.import binary "sintab.bin"

colourList:
    .byte 2,2,2,8,8,8,9,9,9,1,1,1,9,9,9,8,8,8,2,2,2,5,5,5,3,3,3,13,13,13,1,1,1,13,13,13,13,3,3,3,5,5,5,6,6,6,10,10,10,14,14,14,2,2,2,14,14,14,10,10,10,6,6,6
colourListEnd:

* = spriteBase "Sprites"  // 8 sprites $3800-$39ff
SpriteStart: 
.fill 64*8,$ff // fill sprite data all pixels on


// SPRITE IMAGE DATA : 8 images : total size is 512 ($200) bytes. E8

spr_img0:

.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$28,$14,$c3,$82,$41,$c3,$82,$40,$cc,$82,$40,$cc,$82,$40
.byte $f0,$82,$14,$f0,$82,$01,$cc,$82,$01,$cc,$82,$41,$c3,$28,$14,$c3
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$81

spr_img1:

.byte $0a,$00,$00,$28,$80,$00,$28,$00,$00,$a0,$00,$00,$a0,$20,$00,$80
.byte $a0,$00,$00,$94,$00,$02,$91,$00,$22,$10,$00,$0a,$00,$00,$01,$10
.byte $40,$01,$00,$f0,$00,$00,$c0,$00,$07,$c0,$00,$57,$00,$00,$1f,$f3
.byte $00,$0c,$f0,$00,$0c,$c0,$00,$00,$c0,$00,$00,$c0,$00,$00,$c0,$81

spr_img2:

.byte $02,$00,$00,$02,$00,$00,$02,$00,$00,$02,$00,$00,$00,$aa,$00,$00
.byte $aa,$00,$00,$05,$00,$00,$05,$00,$01,$10,$00,$01,$10,$00,$01,$10
.byte $00,$01,$10,$00,$00,$41,$00,$00,$41,$00,$03,$ff,$00,$03,$ff,$00
.byte $00,$30,$00,$00,$30,$00,$00,$cc,$00,$00,$cc,$00,$03,$03,$00,$81

spr_img3:

.byte $00,$00,$28,$00,$08,$28,$00,$08,$0a,$00,$08,$0a,$00,$1a,$00,$00
.byte $52,$00,$00,$02,$88,$00,$01,$80,$01,$01,$80,$0d,$51,$40,$0f,$40
.byte $00,$03,$00,$00,$03,$c1,$00,$c3,$c4,$00,$ff,$f0,$00,$00,$30,$00
.byte $03,$30,$00,$03,$00,$00,$00,$00,$00,$03,$00,$00,$03,$00,$00,$81

spr_img4:

.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$c3,$14,$28,$c3
.byte $41,$82,$33,$40,$82,$33,$40,$82,$0f,$14,$82,$0f,$01,$82,$33,$01
.byte $82,$33,$01,$82,$c3,$41,$82,$c3,$14,$28,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$81

spr_img5:

.byte $00,$00,$00,$03,$c0,$00,$00,$0c,$00,$3c,$3c,$00,$3c,$f4,$00,$00
.byte $f4,$00,$00,$c4,$40,$03,$c0,$00,$03,$04,$40,$03,$40,$20,$00,$40
.byte $28,$00,$41,$a0,$00,$05,$82,$00,$16,$80,$00,$06,$02,$00,$02,$02
.byte $00,$00,$0a,$00,$02,$0a,$00,$00,$28,$00,$00,$a8,$00,$00,$20,$81

spr_img6:

.byte $00,$33,$00,$00,$33,$00,$00,$0c,$00,$00,$0c,$00,$00,$ff,$c0,$00
.byte $ff,$c0,$00,$01,$40,$00,$01,$40,$00,$40,$00,$00,$40,$00,$00,$40
.byte $00,$00,$40,$00,$00,$14,$40,$00,$14,$40,$00,$2a,$80,$00,$2a,$80
.byte $00,$80,$00,$00,$80,$00,$00,$80,$00,$00,$80,$00,$00,$2a,$80,$81

spr_img7:

.byte $00,$00,$30,$00,$00,$00,$00,$00,$30,$00,$00,$30,$00,$0f,$0c,$00
.byte $07,$3f,$00,$17,$cc,$00,$00,$c0,$00,$40,$f0,$00,$40,$70,$00,$40
.byte $70,$02,$54,$40,$0a,$90,$00,$00,$80,$40,$20,$a0,$00,$20,$24,$00
.byte $20,$28,$00,$28,$00,$00,$28,$00,$00,$0a,$20,$00,$0a,$00,$00,$81


