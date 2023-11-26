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

* = $02 "zeropage" virtual
bounceOn: .byte 0
colourBarsOn: .byte 0
sinPosition: .byte 0
nextchar: .byte 0
spriteColour: .byte 0
colourBarPointer: .byte 0

* = $0801

BasicUpstart2(Start)

Start: {

        sei
		//Stop CIA interrupts
		lda #$7f
		sta $dc0d
		sta $dd0d

		//Bank out BASIC + Kernal
		lda #$35
		sta $01


		// Set Fore/back colors and clear screen to black 
        // we can also set up some inital variables along the way while we have
        // the correct number in a
		lda #$00
        sta bounceOn
        sta colourBarsOn
        sta sinPosition
        sta colourBarPointer
        sta $d020
        sta $d021
        lda #1              
        sta spriteColour    //start sprite colour as white (really background since sprites are transparent)

        lda #<scrolltext
        sta PutCharInSprite7.scrolltextPointer
        lda #>scrolltext
        sta PutCharInSprite7.scrolltextPointer+1
		jsr ClearScreen

        ldx #7
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

        lda #$ff
        sta $d01d       //set X double width
        lda #0
        sta $d017       // set Y to normal height
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

        lda #$FE        //sprites 1-7
        sta $d015       //turn on sprites 1 - 7
        jsr PutCharInSprite7    //put first char of our text into sprite 7

forever:
    eof:
        lda $d011
        bpl eof   // wait to below line 256
    
        // do all the hard bits while we have loads of time
        jsr bounceIt        // do the bouncy thing if enabled

        jsr scrollSprites   // move everything left a bit - takes 18 lines!

topbit:
        lda $d011   //wait until we get back to top part of screen
        bmi topbit


        lda colourBarsOn        
        beq justOneColour

    // do colour bars here
        jmp forever

    justOneColour:
        jsr SetSpriteColour
        jmp forever         //keep doing it again and again until user gets bored and closes vice64
        rts                 //obligatory rts because you never know
}

SetSpriteColour:
{
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
}

bounceIt:
{
    lda bounceOn
    beq exit
    inc sinPosition
    ldx sinPosition

    lda sinBase,x
    ldx #0 
bounce1:  
    sta $d001,x     //set sprite Y
    inx
    inx             // move to next sprite
    cpx #$10        // have we done all 8?
    bne bounce1     // not yet ^
exit:
    rts
}

PutCharInSprite7:
{
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
    bcs colourbars
    and #$3f
    sta spriteColour
    jmp getCharacter
colourbars:                 //test for OSK sprite or colourbar line
    //and #1
    //sta colourBarsOn
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
}

scrollSprites:
{
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
    bpl exit        //not moved 8 yet so get out of here
    lda #7          //reset pixel counter
    sta nextchar
    jsr PutCharInSprite7    //put next character into sprite7
exit:    
    rts
}
        
ClearScreen: {
		lda #$20
		ldx #$00
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
}


.label bounce_off = 128
.label bounce_on = 129
.label setColour  = 192
.label oskSpriteOff = 224
.label oskSpriteOn = 225
.label extraTextOff = 226
.label extraTextOn = 227

scrolltext:     // chars above 127 trigger effects on /off  128/9 = bounce off/on, 192 + x = colour, 224/5 = Sprite 0 on /off 226/7 additonal line with colur bars on/off
        .text "OldSkoolCoder gave us homework. A text scroller he said... "
        .byte bounce_on
        .text " We thought it would be fun to add extra things."
        .byte bounce_off
        .text" We can turn bounce on and off."
        .byte bounce_on
        .text " We can change colour. Lets try"
        .byte setColour + 2
        .text " RED(Really? you call this RED), or maybe we want"
        .byte setColour + 7
        .text " YELLOW. I've been told some people like the colour"
        .byte setColour + 14
        .text " Purple? "
        .text "Oh look, i found a spare sprite!"
        .byte oskSpriteOn
        .text "The only thing left to do is"
        .byte extraTextOn
        .text " colour bars. I'm rubbish at colour sequences so you'll have to make do with this."
        .byte bounce_off
        .text "                      "
        .byte setColour + 1
        .byte 0 // text terminator

extraText:
    .text "OSK taught us how to do this."

* = charBase "Charset"
CharSet:
.import binary "464charset-00-7F.bin"   // charset at $3000 - $33ff

* = $3400 "SinTable"  // sin data starting at line 100  $3400-$34ff
SinTable: 
.import binary "sintab.bin"

colourList:
        .byte 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6

* = spriteBase "Sprites"  // 8 sprites $3800-$39ff
SpriteStart: 
.fill 64*8,$ff // fill sprite data all pixels on
