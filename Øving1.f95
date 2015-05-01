
!-------- ØVING 1 --------!



subroutine init_random_seed()
	INTEGER :: i, n, clock
	INTEGER, DIMENSION(:), ALLOCATABLE :: seed

	CALL RANDOM_SEED(size = n)
	ALLOCATE(seed(n))

	CALL SYSTEM_CLOCK(COUNT = clock)

	seed = clock + 37*(/(i-1, i = 1, n) /)
	CALL RANDOM_SEED(PUT = seed)

	DEALLOCATE(seed)


end


program BS	

!-------- Declaration ---------!
	
	PARAMETER (N = 1000, I = 10000)
	DIMENSION species(N), rand(N), minL(1), rand3(3), int3(3), lowest(10), speciepos(I), time(I)
	INTEGER :: j = 1, irr = 1, l1
!	REAL :: rand(N)

	CALL init_random_seed()
	CALL random_number(rand)
	species = INT(rand*1000)
	minL = MINLOC(species)
	do m = 1, 10, 1
		lowest(m) = 0
	end do

!	write (*,*) species
!	write (*,*) 'Lowest number is: ' , species(minL)
!	write (*,*) 'Location: nr.' , minL

! 	open file to load into	
	OPEN (UNIT = 2, FILE ='result.dat')

!------- Run I times -------!	
	DO WHILE (j <= I)
		l1 = minL(1)
		IF (species(l1) <= 100) THEN
			lowest(1) = lowest(1) + 1
		ELSE IF (species(l1) > 100 .AND. species(l1) <= 200) THEN
			lowest(2) = lowest(2) + 1
		ELSE IF (species(l1) > 200 .AND. species(l1) <= 300) THEN
			lowest(3) = lowest(3) + 1
		ELSE IF (species(l1) > 300 .AND. species(l1) <= 400) THEN
			lowest(4) = lowest(4) + 1
		ELSE IF (species(l1) > 400 .AND. species(l1) <= 500) THEN
			lowest(5) = lowest(5) + 1
		ELSE IF (species(l1) > 500 .AND. species(l1) <= 600) THEN
			lowest(6) = lowest(6) + 1
		ELSE IF (species(l1) > 600 .AND. species(l1) <= 700) THEN
			lowest(7) = lowest(7) + 1
		ELSE IF (species(l1) > 700 .AND. species(l1) <= 800) THEN
			lowest(8) = lowest(8) + 1
		ELSE IF (species(l1) > 800 .AND. species(l1) <= 900) THEN
			lowest(9) = lowest(9) + 1
		ELSE IF (species(l1) > 900 .AND. species(l1) <= 1000) THEN
			lowest(10) = lowest(10) + 1
		ELSE
			write (*,*) 'Error'
		END IF ! could been done easier with "do" and "continue"
		
		
		speciepos(j) = l1
		time(j) = j
		CALL init_random_seed()
		CALL random_number(rand3)
		int3 = INT(rand3*1000)
		if (minL(1) == 1) then
			species(N) = int3(1)
			species(1) = int3(2)
			species(2) = int3(3)
		else if (minL(1) == N) then
			species(N-1) = int3(1)
			species(N) = int3(2)
			species(1) = int3(3)
		else
			species(minL-1) = int3(1)
			species(minL) = int3(2)
			species(minL+1) = int3(3)
		end if
		write(2,*) j,minL
		minL = MINLOC(species)
!		write (*,*) 'Lowest number is: ' , species(minL)
!		write (*,*) 'Location: nr.' , minL
		j = j+1
		DO WHILE (irr < 999999)
			irr = irr + 1
		END DO
		irr = 1
		


		
	END DO
	write (*,*) lowest
	write (*,*) 'Total number: ', SUM(lowest)
END







