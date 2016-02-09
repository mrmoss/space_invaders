;HARDNESS LEVEL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%define hardness_level 7							;hardness level (4 - easy, 7 - normal, 15 - hard, 30 - insane)

;bootable x86 space invaders game
;	created by:	charlie carlson, mike moss, and zak williams
;	description:	this is a simple bootable space invaders game, all trademarks belong to their rightful owners.
;	special thanks to dr. orion lawlor for help on pretty much everything, especially for teaching all three of us
;	assembly, or else none of this would have got created!

[bits 16]									;16 bit mode
[org 0x7c00]									;ibm standard for program origin

%define object_size 13								;object size definition - x[WORD],y[WORD],sprite pointer[WORD],
										;x speed[WORD],y speed[WORD],color[BYTE]

%define enemy_num_col 11							;enemy number per column definition
%define enemy_num_row 5								;enemy number per row definition

%define swarm_yspeed 5								;swarm y speed

%define tank_bullet_num 15							;tank bullet number definition
%define enemy_bullet_num 100							;enemy bullet number definition

%define tank_shoot_speed 12							;tank shoot speed definition
%define enemy_shoot_speed 15							;enemy shoot speed definition

%macro set_video_mode 1								;set video mode macro - mode[BYTE]
	push ax									;save ax
	mov al,%1								;video mode
	call fset_video_mode							;call set video mode function
	pop ax									;load ax
	%endmacro								;end of macro

%macro set_pixel 3								;set pixel macro - x[WORD],y[WORD],color[BYTE]
	push ax									;save ax
	push cx									;save cx
	push dx									;save dx
	mov cx,%1								;x
	mov dx,%2								;y
	mov al,%3								;color
	call fset_pixel								;call set pixel function
	pop dx									;save dx
	pop cx									;save cx
	pop ax									;save ax
	%endmacro								;end of macro

%macro get_pixel 2								;get pixel macro - x[WORD],y[WORD],color[BYTE](RIGHTS INTO COLOR)
	push cx									;save cx
	push dx									;save dx
	mov cx,%1								;x
	mov dx,%2								;y
	call fget_pixel								;call get pixel function
	pop dx									;save dx
	pop cx									;save cx
	%endmacro								;end of macro

%macro draw_sprite 4								;draw sprite macro - sprite pointer[WORD],x[WORD],y[WORD],color[BYTE]
	pusha									;save all registers
	mov di,%1								;sprite pointer
	mov cx,%2								;x
	mov dx,%3								;y
	mov al,%4								;color
	call fdraw_sprite							;call draw sprite function
	popa									;load all registers
	%endmacro								;end of macro

%macro object_clear_trail_x 1							;object_clear_trail_x macro - object pointer[WORD]
	push di									;save di
	mov di,%1								;object pointer
	call fobject_clear_trail_x						;clear x trail
	pop di									;load di
	%endmacro								;end of macro

%macro object_clear_trail_y 1							;object_clear_trail_y macro - object pointer[WORD]
	push di									;save di
	mov di,%1								;object pointer
	call fobject_clear_trail_y						;clear y trail
	pop di									;load di
	%endmacro								;end of macro

%macro create_object 8								;create object macro - object pointer[WORD],x[WORD],y[WORD],
										;sprite pointer[WORD],x speed[WORD],y speed[WORD],color[BYTE],
										;timer value[WORD]
	push di									;save di
	push bp									;save bp
	push ax									;save ax
	mov di,%1								;move into di object pointer
	mov bp,%2								;load x position
	mov WORD[di],bp								;move x into object x
	mov bp,%3								;load y position
	mov WORD[di+2],bp							;move y into object y
	mov bp,%4								;load sprite pointer
	mov WORD[di+4],bp							;move sprite pointer into object sprite pointer
	mov bp,%5								;load x speed
	mov WORD[di+6],bp							;move x speed into object x speed
	mov bp,%6								;load y speed
	mov WORD[di+8],bp							;move y speed into object y speed
	mov ax,%7								;load color
	mov BYTE[di+10],al							;move color into object color
	pop ax									;load ax
	pop bp									;load bp
	pop di									;load di
	%endmacro								;end of macro

boot_loader:									;loads space invaders
	mov ah,0x02								;read sectors argument
	mov al,0x3f								;sector count
	mov ch,0x00								;cylinder
	mov cl,0x02								;sector start
	mov dh,0x00								;head
	mov dl,0x80								;hard drive
	mov di,0x0000								;load segment address into temp register
	mov es,di								;load temp register into segment register
	mov bx,0x7e00								;space invaders start address
	int 0x13								;load sectors
	jmp 0x0000:0x7e00							;go to space invaders

times 510-($-$$) db 0								;fill up space
dw 0xaa55									;bootable signature

jmp main									;goto main game loop

restart:		db 0x00							;restart variable
time_counter:		dw tank_shoot_speed						;timer variable

keyboard_left:		db 0							;left key variable
keyboard_right:		db 0							;right key variable
keyboard_space:		db 0							;space pressed variable
keyboard_space_down:	db 0							;space down variable
keyboard_test:		db 0							;test variable

swarm_x:		dw 1							;swarm x variable
swarm_y:		dw 11							;swarm y variable
swarm_direction:	db 0x00							;swarm direction variable (0x00=left 0xff=right)
swarm_speed:		dw 1							;swarm speed variable
swarm_ycount:		dw 0							;swarm times till speed increase

;sprites - width(WORD),height(WORD),data(BYTE(S))
	sprite_tank:
		dw 10
		dw 10
		db 0x00,0x00,0x00,0x00,0x0f,0x0f,0x00,0x00,0x00,0x00
		db 0x00,0x00,0x00,0x00,0x0f,0x0f,0x00,0x00,0x00,0x00
		db 0x00,0x00,0x00,0x00,0x0f,0x0f,0x00,0x00,0x00,0x00
		db 0x00,0x00,0x00,0x0f,0x0f,0x0f,0x0f,0x00,0x00,0x00
		db 0x00,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x00
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
	sprite_enemy:
		dw 11
		dw 8
		db 0x00,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x0f,0x00,0x00
		db 0x00,0x00,0x00,0x0f,0x00,0x00,0x00,0x0f,0x00,0x00,0x00
		db 0x00,0x00,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x00,0x00
		db 0x00,0x0f,0x0f,0x00,0x0f,0x0f,0x0f,0x00,0x0f,0x0f,0x00
		db 0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f
		db 0x0f,0x00,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x0f,0x00,0x0f
		db 0x0f,0x00,0x0f,0x00,0x00,0x00,0x00,0x00,0x0f,0x00,0x0f
		db 0x00,0x00,0x00,0x0f,0x0f,0x00,0x0f,0x0f,0x00,0x00,0x00
	sprite_bullet:
		dw 2
		dw 4
		db 0x0f,0x0f
		db 0x0f,0x0f
		db 0x0f,0x0f
		db 0x0f,0x0f

;objects
	tank:									;tank object
		dw 154								;x
		dw 182								;y
		dw sprite_tank							;sprite pointer
		dw 2								;x speed
		dw 5								;y speed
		db 0x02								;color
	enemies:								;enemy objects
		times (enemy_num_col*enemy_num_row)*object_size db 0		;total enemies (55 regulars)
	tank_bullets:								;tank bullet objects
		times (tank_bullet_num)*object_size db 0			;total bullets
	enemy_bullets:								;enemy bullet objects
		times (enemy_bullet_num)*object_size db 0			;total bullets

main:										;main game loop
	mov ax,0x0000								;move timer function segment address into temporary register
	mov es,ax								;move temporary register into es
	mov WORD[es:0x001c*4+0],timer						;load custom timer function into interrupt table
	mov WORD[es:0x001c*4+2],0x0000						;load custom segment into segment section of interrupt table
	mov ax,0x0000								;move keyboard function segment address into temporary register
	mov es,ax								;move temporary register into es
	mov WORD[es:0x0015*4],keyboard_hook					;load custom keyboard function into interrupt table
	mov WORD[es:0x0015*4+2],0x0000						;load custom segment into segment section of interrupt table
	set_video_mode 0x13							;set video mode
	call create_enemies							;create enemies
	jmp $									;hang here

timer:										;custom timer interrupt
	add WORD[time_counter],1						;increment timer
	call update_objects							;update objects
	mov BYTE[keyboard_space],0x00						;clear space pressed
	iret									;return from interrupt

keyboard_hook:									;custom keyboard book interrupt
	jmp keyboard_hook_check							;jump to check function
	keyboard_hook_right_down:						;keyboard right down function
		mov BYTE[keyboard_right],0xff					;set right key
		jmp keyboard_hook_end						;goto end
	keyboard_hook_left_down:						;keyboard left down function
		mov BYTE[keyboard_left],0xff					;set left key
		jmp keyboard_hook_end						;goto end
	keyboard_hook_space_down:						;keyboard space down function
		cmp BYTE[keyboard_space_down],0x00				;compare space down
		jne keyboard_hook_space_down_skip				;if true goto skip
		mov BYTE[keyboard_space],0xff					;else set space pressed
		keyboard_hook_space_down_skip:					;skip label
		mov BYTE[keyboard_space_down],0xff				;set space down
		jmp keyboard_hook_end						;goto end
	keyboard_hook_right_up:							;keyboard right up function
		mov BYTE[keyboard_right],0x00					;set right key
		jmp keyboard_hook_end						;goto end
	keyboard_hook_left_up:							;keyboard left up function
		mov BYTE[keyboard_left],0x00					;set left key
		jmp keyboard_hook_end						;goto end
	keyboard_hook_space_up:							;keyboard space up function
		mov BYTE[keyboard_space_down],0x00				;set space down
		mov BYTE[keyboard_space],0x00					;set space pressed
		jmp keyboard_hook_end						;goto end
	keyboard_hook_check:							;check function
		cmp al,0x4d							;check right down
		je keyboard_hook_right_down					;goto right down function
		cmp al,0x4b							;check left down
		je keyboard_hook_left_down					;goto left down function
		cmp al,0x39							;check space down
		je keyboard_hook_space_down					;goto space down function
		cmp al,0xcd							;check right up
		je keyboard_hook_right_up					;goto right up function
		cmp al,0xcb							;check left up
		je keyboard_hook_left_up					;goto left up function
		cmp al,0xb9							;check space up
		je keyboard_hook_space_up					;goto space up function
	keyboard_hook_end:							;end function
		iret								;return from interrupt

create_enemies:									;fake enemy drawing function
	push ax									;save ax
	push bx									;save bx
	push dx									;save cx
	mov ax,0								;set row counter to 0
	mov cx,enemies								;move into cx enemies pointer
	jmp create_enemies_row_check						;jump to row check
	create_enemies_row_function:						;row function
		mov bx,0							;set row counter to 0
		push ax								;save ax
		imul ax,13							;multiply row counter by 13
		add ax,WORD[swarm_y]						;add to column counter swarm y position
		jmp create_enemies_col_check					;jump to column check
		create_enemies_col_function:					;column function
			push bx							;save bx
			imul bx,16						;multiply column counter by 16
			add bx,WORD[swarm_x]					;add to column counter swarm x position
		create_object cx,bx,ax,sprite_enemy,WORD[swarm_speed],0,0x0f,0	;create enemy
			pop bx							;load bx
			add bx,1						;increment column counter
			add cx,object_size					;increment enemy pointer
		create_enemies_col_check:					;column check
			cmp bx,enemy_num_col					;compare column counter to number of columns
			jl create_enemies_col_function				;if less than draw column
		pop ax								;load row
		add ax,1							;increment row
	create_enemies_row_check:						;row check
		cmp ax,enemy_num_row						;compare row counter to number of rows
		jl create_enemies_row_function					;if less than draw row
	pop cx									;load cx
	pop bx									;load bx
	pop ax									;load ax
	ret									;return

update_tank:									;update tank function
	cmp BYTE[keyboard_right],0xff						;check keyboard right
		jne timer_right_check_skip					;if not set skip
		call tank_move_right						;else move tank right
	timer_right_check_skip:							;skip label
		cmp BYTE[keyboard_left],0xff					;check keyboard left
		jne timer_left_check_skip					;if not set skip
		call tank_move_left						;else move tank left
	timer_left_check_skip:							;skip label
		cmp BYTE[keyboard_space],0xff					;check keyboard space pressed
		jne timer_space_check_skip					;if not set skip
		call tank_shoot_timer						;else call shoot timer
	timer_space_check_skip:							;skip label
		object_clear_trail_x tank					;clear tank x trail
		object_clear_trail_y tank					;clear tank y trail
		draw_sprite WORD[tank+4],WORD[tank],WORD[tank+2],BYTE[tank+10]	;draw tank
	push si									;save si
	mov si,tank								;move into si tank pointer
	call check_enemy_bullets						;check for collisions with enemy bullets
	pop si									;load si
	ret									;return

tank_move_right:								;tank move right function
	push di									;save di
	mov di,WORD[tank+4]							;move into di sprite pointer of tank
	push bx									;save bx
	mov bx,319								;move into bx screen width - 1
	sub bx,WORD[di]								;subtract from bx width of sprite of tank
	cmp WORD[tank],bx							;compare x position to bx
	pop bx									;load bx
	pop di									;load di
	jge tank_move_right_skip						;if greater than or equal to skip
	push ax									;save ax
	mov ax,WORD[tank+6]							;move into ax x speed of tank
	add WORD[tank],ax							;add ax to x position of tank
	pop ax									;load ax
	tank_move_right_skip:							;skip label
		ret								;return

tank_move_left:									;tank move left function
	cmp WORD[tank],1							;compare tank x position to 1
	jle tank_move_left_skip							;if less than or equal to skip
	push ax									;save ax
	mov ax,WORD[tank+6]							;move into ax x speed of tank
	sub WORD[tank],ax							;sub ax from x position of tank
	pop ax									;load ax
	tank_move_left_skip:							;skip label
		ret								;return

tank_shoot:									;tank shoot function
	push di									;save di
	mov di,tank_bullets							;move into di tank_bullets pointer
	push ax									;save ax
	mov ax,0								;move into ax bullet counter
	jmp tank_shoot_check							;jump to check function
	tank_shoot_function:							;function function
		add di,ax							;add to di bullet counter
		cmp WORD[di+4],0						;compare sprite pointer to 0 (check if allocated)
		jne tank_shoot_skip						;if allocated check next bullet
		push ax								;save ax
		push bx								;save bx
		push cx								;save cx
		mov bx,WORD[tank]						;move into bx tank x position
		add bx,5-1							;add half of tank sprite width subtract half of 
										;bullet width (move bullet to middle of tank)
		mov cx,WORD[tank+2]						;move into cx tank y position
		sub cx,8							;subtract (sprite height + (y speed * 2)) (move bullet above tank)
		push ax								;save ax
		mov ah,0							;clear ah
		mov al,BYTE[tank+10]						;put tank color into al
		create_object di,bx,cx,sprite_bullet,0,2,ax,0			;allocate bullet
		pop ax								;load ax
		pop cx								;load cx
		pop bx								;load bx
		pop ax								;load ax
		jmp tank_shoot_end						;jump to end function
		tank_shoot_skip:						;skip label
			sub di,ax						;remove ax from di
			add ax,object_size					;increment ax to point to next bullet
	tank_shoot_check:							;check function
		cmp ax,tank_bullet_num*object_size				;compare counter to end iterator
		jb tank_shoot_function						;if less than fo to function function
	tank_shoot_end:								;end function
		pop ax								;load ax
		pop di								;load di
		ret								;return

tank_shoot_timer:								;tank shoot_timer function
	cmp WORD[time_counter],tank_shoot_speed					;check time
	jl tank_shoot_timer_skip						;if less skip
	call tank_shoot								;make tank shoot
	mov WORD[time_counter],0						;reset time
	tank_shoot_timer_skip:							;skip label
		ret								;return

update_enemies:									;update enemies function
	push cx									;save cx
	mov cx,0								;set enemies updated counter to 0
	push bp									;save bp
	mov bp,0								;set counter to 0
	push ax									;save ax
	mov ax,WORD[swarm_speed]						;move speed into ax
	cmp BYTE[swarm_direction],0xff						;if swarm direction is right move swarm x position right
	jne update_enemies_swarm_go_left					;else go left
	add WORD[swarm_x],ax							;add ax to swarm x position
	jmp update_enemies_swarm_skip_left					;skip going left
	update_enemies_swarm_go_left:						;skip label
		sub WORD[swarm_x],ax						;subtract ax from swarm x position
	update_enemies_swarm_skip_left:						;skip label
		pop ax								;load ax
		cmp WORD[swarm_x],0						;compare swarm x position with left boundry
		jge update_enemies_bound_left_func_skip				;if greater skip
		mov BYTE[swarm_direction],0xff					;else change direction
		add bp,swarm_yspeed						;put swarm y speed into bp
		add WORD[swarm_y],bp						;add to swarm y position swarm y speed
		call check_swarm_count						;check swarm's y position for speed changes
	update_enemies_bound_left_func_skip:					;skip label
		push ax								;save ax
		mov ax,149							;mov into ax the width of swarm
		sub ax,WORD[swarm_speed]					;subtract from ax the swarm x speed
		cmp WORD[swarm_x],ax						;compare swarm x position to ax
		pop ax								;load ax
		jl update_enemies_bound_right_func_skip				;if less than right boundry skip
		mov BYTE[swarm_direction],0x00					;else change direction
		add bp,swarm_yspeed						;put swarm y speed into bp
		add WORD[swarm_y],bp						;add to swarm y position swarm y speed
		call check_swarm_count						;check swarm's y position for speed changes
	update_enemies_bound_right_func_skip:					;skip label
		push si								;save si
		mov si,0							;set counter to 0
		jmp update_enemies_check					;jump to check
	update_enemies_function:						;function
		push si								;save si
		add si,enemies							;add enemies pointer location
		add WORD[si+11],1						;increment enemy shoot timer
		cmp WORD[si+4],0						;compare sprite to 0
		je update_enemies_function_skip					;if 0 (or unallocated) then skip update
		add cx,1							;increment enemies updated counter
		call check_tank_bullets						;check for collisions with tank bullets
		add WORD[si+2],bp						;add to enemy y position swarm y speed
		mov WORD[si+8],bp						;move into enemy y speed swarm y speed
		cmp WORD[si+2],171						;compare y position to tank top boundry
		jl update_enemies_bound_bottom_func_skip			;if less than skip
		mov BYTE[restart],0xff						;else set restart byte to true
		update_enemies_bound_bottom_func_skip:				;skip label
			push di							;save di
			push ax							;save ax
			call get_free_bullet					;get free bullet
			cmp di,0						;check if there was a free bullet
			je update_enemies_create_bullet_skip			;if not skip
			cmp WORD[si+11],enemy_shoot_speed			;compare enemy shoot timer to shoot time
			jl update_enemies_create_bullet_skip			;if less skip
			mov WORD[si+11],0					;else reset timer
			push ax							;save ax
			mov ax,WORD[tank]					;move into ax tank x position
			sub ax,WORD[swarm_speed]				;subtract swarm speed from tank x position
			sub ax,hardness_level					;subtract hardness buffer
			cmp WORD[si],ax						;compare tank left boundry to enemy right boundry
			pop ax							;load ax
			jl update_enemies_create_bullet_skip			;if not in range do not shoot
			push ax							;save ax
			mov ax,WORD[tank]					;move into ax tank x position
			add ax,WORD[swarm_speed]				;add swarm speed to tank x position
			add ax,hardness_level					;add hardness buffer
			cmp WORD[si],ax						;compare tank right boundry to enemy left boundry
			pop ax							;load ax
			jg update_enemies_create_bullet_skip			;if not in range do not shoot
			push di							;save di
			mov di,si						;move into di the enemy pointer
			add di,(enemy_num_col*object_size)			;add to di 1 row of enemies
			cmp WORD[di+4],0					;check enemy below
			pop di							;load di
			jne update_enemies_create_bullet_skip			;if enemy below exists skip
			jmp update_enemies_create_bullet_not_last_row_skip	;if passed skip next check
			push ax							;save ax
			push bx							;save bx
			mov ax,enemies+(enemy_num_row*enemy_num_col*object_size);move into ax the last enemy pointer
			mov bx,si						;mov into bx the current enemy pointer
			add bx,(enemy_num_col*object_size)			;add a row of enemies to the current enemy pointer
			cmp bx,ax						;compare last enemy to current enemy + a row
			pop bx							;load bx
			pop ax							;load ax
			jb update_enemies_create_bullet_skip			;if below skip
			update_enemies_create_bullet_not_last_row_skip:		;skip label
			mov al,BYTE[si+10]					;move into al the color of enemy
			mov ah,0						;clear ah
		create_object di,WORD[si],WORD[si+2],sprite_bullet,0,2,ax,0	;create enemy bullet
		update_enemies_create_bullet_skip:				;skip label
			pop ax							;load ax
			pop di							;load di
			object_clear_trail_y si					;cover y trail
			push ax							;save ax
			mov ax,WORD[swarm_speed]				;move swarm speed into ax
			mov WORD[si+6],ax					;move ax into enemy x speed
			cmp BYTE[swarm_direction],0xff				;if swarm direction is right move x position right
			jne update_enemies_go_left				;else go left
			add WORD[si],ax						;add ax to x position
			jmp update_enemies_skip_left				;skip going left
		update_enemies_go_left:						;skip label
			sub WORD[si],ax						;subtract ax from x position
		update_enemies_skip_left:					;skip label
			pop ax							;load ax
			object_clear_trail_x si					;cover x trail
			mov WORD[si+8],0					;move zero into the y speed of enemy
			push ax							;save ax
			mov al,BYTE[si+10]					;move color into register (can't put directly into macro)
			draw_sprite WORD[si+4],WORD[si],WORD[si+2],al		;draw enemy
			pop ax							;load ax
			update_enemies_function_skip:				;skip label
				pop si						;load si
				add si,object_size				;increment counter
	update_enemies_check:							;check
		cmp si,(enemy_num_row*enemy_num_col*object_size)		;compare counter to number of enemies
		jb update_enemies_function					;if less than go to function
		pop si								;load si
		pop bp								;load bp
		cmp cx,0
		pop cx								;load cx
		je game_over
		ret								;return

check_swarm_count:								;swarm increase speed function
	cmp WORD[swarm_speed],5							;compare swarm speed to 5
	jge check_swarm_count_end_skip						;cap swarm speed to 5
	cmp WORD[swarm_ycount],4						;compare swarm count to 4
	jl check_swarm_count_end						;if less than add to count
	mov WORD[swarm_ycount],0						;else reset swarm count
	add WORD[swarm_speed],1							;increase speed
	check_swarm_count_end:							;end label
	add WORD[swarm_ycount],1						;add to y count
	check_swarm_count_end_skip:						;skip label
	ret									;return

check_tank_bullets:								;tank bullet check function
	push di									;save di
	mov di,tank_bullets							;move into di tank bullet pointer
	jmp check_tank_bullets_check						;go to check
	check_tank_bullets_function:						;function
		push ax								;save ax
		mov ax,WORD[di]							;move into ax bullet x position
		add ax,1							;add to x position 1
		cmp ax,WORD[si]							;compare bullet right boundry to enemy left boundry
		jl check_tank_bullets_fail					;if less go to fail
		push di								;save di
		mov di,WORD[si+4]						;move into di the sprite of enemy
		sub ax,WORD[di]							;subtract from ax the width of the sprite of the enemy
		sub ax,1							;subtract 1 from ax
		cmp ax,WORD[si]							;compare right boundry of enemy to left bullet boundry
		pop di								;load di
		jg check_tank_bullets_fail					;if greater go to fail
		mov ax,WORD[di+2]						;move into ax the bullet's y position
		cmp ax,WORd[si+2]						;compare top boundry to bullet's top boundry
		jl check_tank_bullets_fail					;if less go to fail
		push di								;save di
		mov di,WORD[si+4]						;move into di the sprite of enemy
		sub ax,WORD[di+2]						;subtract from ax the height of sprite of the enemy
		cmp ax,WORD[si+2]						;compare top boundry of bullet to bottom boundry of enemy
		pop di								;load di
		jg check_tank_bullets_fail					;if greater go to fail
		pop ax								;load ax
		draw_sprite WORD[si+4],WORD[si],WORD[si+2],0x00			;clear enemy sprite
		push si								;save di
		mov si,di							;move into si bullet
		push ax								;save ax
		mov ax,WORD[si+2]						;move into ax the y position of bullet
		add ax,WORD[si+8]						;add to ax the y speed of bullet
		draw_sprite WORD[si+4],WORD[si],ax,0x00				;clear bullet
		pop ax								;load ax
		pop si								;load si
		create_object si,0,0,0,0,0,0,0					;deallocate enemy
		create_object di,0,0,0,0,0,0,0					;deallocate bullet
		jmp check_tank_bullets_end					;go to end
		check_tank_bullets_fail:					;fail label
			pop ax							;load ax
			add di,object_size					;increment bullet pointer
	check_tank_bullets_check:						;check label
		cmp di,tank_bullets+(object_size*tank_bullet_num)		;compare counter to last bullet
		jb check_tank_bullets_function					;if less go to function
	check_tank_bullets_end:							;end label
		pop di								;load di
		ret								;return

update_tank_bullets:								;update tank_bullets function
	push ax									;save ax
	mov ax,0								;set counter to 0
	jmp update_tank_bullet_check						;go to check function
	update_tank_bullet_deallocate:						;deallocate function
		object_clear_trail_y si						;clear y trail
		draw_sprite WORD[si+4],WORD[si],WORD[si+2],0x00			;clear sprite
		create_object si,0,0,0,0,0,0,0					;set bullet to all zeros
		pop si								;load si
		jmp update_tank_bullet_end					;jump to end function
	update_tank_bullet_function:						;function function
		push si								;load si
		mov si,ax							;move counter into si
		imul si,object_size						;multiply si by size of an object
		add si,tank_bullets						;add bullet memory location to si
		cmp WORD[si+2],0						;compare the y position to zero
		jl update_tank_bullet_deallocate				;if less than then deallocate bullet
		cmp WORD[si+4],0						;compare sprite to zero (if unallocated)
		je update_tank_bullet_function_skip				;if unallocated skip drawing
		object_clear_trail_x si						;clear x trail
		object_clear_trail_y si						;clear y trail
		draw_sprite WORD[si+4],WORD[si],WORD[si+2],BYTE[si+10]		;draw sprite
		push ax								;save ax
		mov ax,WORD[si+8]						;move y speed into ax
		sub WORD[si+2],ax						;move bullet up
		pop ax								;load ax
		update_tank_bullet_function_skip:				;skip label
			add ax,1						;increment counter
			pop si							;load si
	update_tank_bullet_check:						;check function
		cmp ax,tank_bullet_num						;compare counter to tank_bullet_num
		jb update_tank_bullet_function					;if less go to function function
	update_tank_bullet_end:							;end function
		pop ax								;load ax
		ret								;return

check_enemy_bullets:								;enemy bullet check function
	push di									;save di
	mov di,enemy_bullets							;move into di bullet pointer
	jmp check_enemy_bullets_check						;go to check
	check_enemy_bullets_function:						;function
		push ax								;save ax
		mov ax,WORD[di]							;move into ax bullet x position
		add ax,1							;add to x position 1
		cmp ax,WORD[si]							;compare bullet right boundry to enemy left boundry
		jl check_enemy_bullets_fail					;if less go to fail
		push di								;save di
		mov di,WORD[si+4]						;move into di the sprite of enemy
		sub ax,WORD[di]							;subtract from ax the width of the sprite of the enemy
		sub ax,1							;subtract 1 from ax
		cmp ax,WORD[si]							;compare right boundry of enemy to left bullet boundry
		pop di								;load di
		jg check_enemy_bullets_fail					;if greater go to fail
		mov ax,WORD[di+2]						;move into ax the bullet's y position
		push di								;save di
		mov di,WORD[di+4]						;move into di the sprite of bullet
		add ax,WORD[di+2]						;add to ax the height of sprite of bullet sprite
		pop di								;load di
		cmp ax,WORD[si+2]						;compare bottom boundry of bullet to bottom boundry of enemy
		jl check_enemy_bullets_fail					;if less than go to fail
		push di								;save di
		mov di,WORD[si+4]						;move into di the sprite of enemy
		sub ax,WORD[di+2]						;subtract from ax the height of sprite of the enemy
		cmp ax,WORD[si+2]						;compare top boundry of bullet to bottom boundry of enemy
		pop di								;load di
		jg check_enemy_bullets_fail					;if greater go to fail
		pop ax								;load ax
		push si								;save si
		mov si,di							;move into si di (have to do this)
		push ax								;push ax
		mov ax,WORD[si+2]						;move into ax the y position of bullet
		add ax,WORD[si+8]						;add to ax the y speed of bullet
		object_clear_trail_y si						;clear trail of bullet
		draw_sprite WORD[si+4],WORD[si],ax,0x00				;clear bullet sprite
		pop ax								;load ax
		pop si								;load si
		mov BYTE[restart],0xff						;set restart byte to true
		create_object di,0,0,0,0,0,0,0					;deallocate bullet
		jmp check_enemy_bullets_end					;go to end
		check_enemy_bullets_fail:					;fail label
			pop ax							;load ax
			add di,object_size					;increment counter
	check_enemy_bullets_check:						;check label
		cmp di,enemy_bullets+(object_size*enemy_bullet_num)		;compare counter to last bullet
		jl check_enemy_bullets_function					;if less than go to function
	check_enemy_bullets_end:						;end label
		pop di								;load di
		ret								;return

update_enemy_bullets:								;update enemy_bullets function
	push si									;save ax
	mov si,0								;set counter to 0
	jmp update_enemy_bullet_check						;go to check function
	update_enemy_bullet_deallocate:						;deallocate function
		object_clear_trail_y si						;clear y trail
		draw_sprite WORD[si+4],WORD[si],WORD[si+2],0x00			;clear sprite
		create_object si,0,0,0,0,0,0,0					;set bullet to all zeros
		jmp update_enemy_bullet_function_skip				;jump to skip
	update_enemy_bullet_function:						;function function
		add si,enemy_bullets						;add bullet memory location to si
		cmp WORD[si+4],0						;compare sprite to zero (if unallocated)
		je update_enemy_bullet_function_skip				;if unallocated skip drawing
		cmp WORD[si+2],199						;compare bullet lower y boundry to bottom of screen
		jge update_enemy_bullet_deallocate				;if greater or equal deallocate bullet
		object_clear_trail_x si						;clear x trail
		object_clear_trail_y si						;clear y trail
		draw_sprite WORD[si+4],WORD[si],WORD[si+2],BYTE[si+10]		;draw sprite
		push ax								;save ax
		mov ax,WORD[si+8]						;move y speed into ax
		add WORD[si+2],ax						;move bullet down
		pop ax								;load ax
		update_enemy_bullet_function_skip:				;skip label
			sub si,enemy_bullets					;subtract enemy_bullets from addition before
			add si,object_size					;increment counter
	update_enemy_bullet_check:						;check function
		cmp si,(enemy_bullet_num*object_size)				;compare counter to enemy_bullet_num
		jl update_enemy_bullet_function					;if less go to function function
		pop si								;load ax
		ret								;return

update_objects:									;update objects function
	call update_enemies							;update enemies
	call update_tank							;update tank
	call update_tank_bullets						;update tank bullets
	call update_enemy_bullets						;update enemy bullets
	cmp BYTE[restart],0xff							;check restart byte
	je game_over								;if true restart game
	ret									;return

fset_video_mode:								;set video mode	function - mode[AL]
	mov ah,0x00								;set video mode argument
	int 0x10								;set video mode
	ret									;return

fset_pixel:									;set pixel function - x[CX],y[DX],color[AL]
	mov ah,0x0c								;write pixel argument
	int 0x10								;write pixel
	ret									;return

fget_pixel:									;get pixel function - x[CX],y[DX],returns color in al!!! TRASHES AX
	mov ah,0x0d								;read pixel argument
	int 0x10								;read pixel
	ret									;return

fobject_clear_trail_x:								;object clear trail x function - object pointer[DI]
	push si									;save si
	push bx									;save bx
	mov si,WORD[di+4]							;get sprite pointer
	mov bx,0								;x counter
	jmp fobject_clear_trail_x_check						;jump x check function
	fobject_clear_trail_x_function:						;x function
		push dx								;save dx
		mov dx,0							;set y counter 0
		jmp fobject_clear_trail_x_function_check			;jump x check function check function
		fobject_clear_trail_x_function_function:			;x check function function
			push ax							;save ax
			mov ax,WORD[di]						;move into ax object x position
			sub ax,bx						;subtract from ax object x speed
			sub ax,1						;one off fix
			push ax							;save ax
			mov ax,WORD[di]						;move into ax object x position
			add ax,WORD[si]						;add to ax sprite width
			add ax,bx						;subtract from ax object x speed
			add dx,WORD[di+2]					;temporary add object y position to y counter
			set_pixel ax,dx,0x00					;set pixel to right
			pop ax							;change pixel position
			set_pixel ax,dx,0x00					;set pixel to left
			sub dx,WORD[di+2]					;remove temporary add to object y counter
			pop ax							;load ax
			add dx,1						;increment y counter
			jmp fobject_clear_trail_x_function_check		;jump to x check function
		fobject_clear_trail_x_function_check:				;x check function check function
			cmp dx,WORD[si+2]					;compare y counter to sprite height
			jl fobject_clear_trail_x_function_function		;if less jump to x check func
			pop dx							;load dx
			add bx,1						;increment x counter
			jmp fobject_clear_trail_x_check				;jump to x check function
	fobject_clear_trail_x_check:						;x check function
		cmp bx,WORD[di+6]						;compare x counter to object x speed
		jl fobject_clear_trail_x_function				;jump if less than to x function
		pop bx								;load bx
		pop si								;load si
		ret								;return

fobject_clear_trail_y:								;object clear trail y function - object pointer[DI]
	push si									;save si
	push bx									;save bx
	mov si,WORD[di+4]							;get sprite pointer
	mov bx,0								;y counter
	jmp fobject_clear_trail_y_check						;jump y check function
	fobject_clear_trail_y_function:						;y function
		push dx								;save dx
		mov dx,0							;set x counter 0
		jmp fobject_clear_trail_y_function_check			;jump y check function check function
		fobject_clear_trail_y_function_function:			;x check function function
			push ax							;save ax
			mov ax,WORD[di+2]					;move into ax object y position
			sub ax,bx						;subtract from ax object y speed
			sub ax,1						;one off fix
			push ax							;save ax
			mov ax,WORD[di+2]					;move into ax object y position
			add ax,WORD[si+2]					;add to ax sprite height
			add ax,bx						;subtract from ax object y speed
			add dx,WORD[di]						;temporary add object x position to x counter
			set_pixel dx,ax,0x00					;set pixel to right
			pop ax							;change pixel position
			set_pixel dx,ax,0x00					;set pixel to left
			sub dx,WORD[di]						;remove temporary add to object x counter
			pop ax							;load ax
			add dx,1						;increment x counter
			jmp fobject_clear_trail_y_function_check		;jump to y check function
		fobject_clear_trail_y_function_check:				;y check function check function
			cmp dx,WORD[si]						;compare x counter to sprite height
			jl fobject_clear_trail_y_function_function		;if less jump to y check func
			pop dx							;load dx
			add bx,1						;increment y counter
			jmp fobject_clear_trail_y_check				;jump to y check function
	fobject_clear_trail_y_check:						;y check function
		cmp bx,WORD[di+8]						;compare y counter to object y speed
		jl fobject_clear_trail_y_function				;jump if less than to y function
		pop bx								;load bx
		pop si								;load si
		ret								;return

fdraw_sprite:									;draw sprite function - sprite pointer[DI],x[CX],y[DX],color[AL]
	mov bx,cx								;move original x position into cx
	mov si,dx								;move original y position into si
	jmp fdraw_sprite_check							;jump to checker label
	fdraw_sprite_function:							;function label
		push ax								;save ax
		push dx								;save dx
		sub dx,si							;dx=dx-si
		imul dx,WORD[di]						;dx=(dx-si)*[di]
		add dx,cx							;dx=((dx-si)*[di])+cx
		sub dx,bx							;dx=((dx-si)*[di])+cx-bx
		add dx,4							;dx=((dx-si)*[di])+cx-bx+4
		push di								;save di
		add di,dx							;di=di+((dx-si)*[di])+cx-bx+4
		and al,BYTE[di]							;change color of pixel
		pop di								;load di
		pop dx								;load dx
		call fset_pixel							;set pixel
		pop ax								;load ax
		add cx,1							;increment x
	fdraw_sprite_check:							;checker label
		push bx								;save bx
		add bx,WORD[di]							;equals original x position + width of sprite
		cmp cx,bx							;check current x position
		pop bx								;load bx
		jl fdraw_sprite_function					;if less draw pixel
		push si								;load si
		add si,WORD[di+2]						;equals original y position + height of sprite
		sub si,1							;fix one off error
		cmp dx,si							;check current y position
		pop si								;load si
		jge fdraw_sprite_end						;if greater than or equal to go to end function
		mov cx,bx							;else reset x position
		add dx,1							;increment y position
		jmp fdraw_sprite_check						;jump to checker label
	fdraw_sprite_end:							;end label
		ret								;return

game_over:									;game over function
	set_video_mode 0x13							;clear screen
	mov ah,0x02								;read sectors argument
	mov al,0x3f								;sector count
	mov ch,0x00								;cylinder
	mov cl,0x02								;sector start
	mov dh,0x00								;head
	mov dl,0x80								;hard drive
	mov di,0x0000								;load segment address into temp register
	mov es,di								;load temp register into segment register
	mov bx,0x7e00								;space invaders start address
	int 0x13								;load sectors
	call create_enemies							;recreate enemies
	ret									;return

get_free_bullet:								;get a free (deallocated) bullet trashes di with pointer to free bullet
	mov di,0								;move into di 0
	jmp get_free_bullet_check						;go to check
	get_free_bullet_function:						;function label
		add di,enemy_bullets						;add to di 
		cmp WORD[di+4],0						;compare bullet sprite with 0 (deallocated or not)
		jne get_free_bullet_skip					;if not free skip
		ret								;else return
		get_free_bullet_skip:						;skip label
			sub di,enemy_bullets					;subtract bullet pointer from addition before
			add di,object_size					;increment bullet pointer
	get_free_bullet_check:							;check label
		cmp di,(enemy_bullet_num*object_size)				;compare counter
		jl get_free_bullet_function					;if less than last bullet go to function
		ret								;return

times 32768-($-$$) db 0								;fill up space
