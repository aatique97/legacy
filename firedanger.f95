!     Rewritten by - Ameera Atique
!     This program computes the fire danger based on certain data

      program firedanger
      implicit none
     
!     Variable declarations

      real:: dry,wet,precip,wind,buo,grass,timber,ffm, df,adfm,fload
      
!     assuming that isnow=1 for snow, isnow=0 for the absence of snow    
      integer:: isnow,iherb 
      
!     Taking all our inputs      
      
      write (*,*) 'Dry Bulb temperature:'
      read  (*,*) dry
      
      write (*,*) 'Wet Bulb temperature:'
      read  (*,*) wet
      
      write (*,*) 'Is there snow?'
      read  (*,*)  isnow
      
      write (*,*) 'Wind Speed'
      read (*,*) wind
      
      write (*,*) 'Build up index:'
      read (*,*) buo
      
      write (*,*) 'Herb State:'
      read (*,*) iherb
      
      write (*,*) 'Precipitation:'
      read (*,*) precip
      
!     Call the subroutine from here 
      call danger(isnow,dry,wet,precip,buo,iherb,grass,timber,ffm,adfm,wind,fload,df)
      end

!     Subroutine danger() starts here
      subroutine danger(isnow,dry,wet,precip,buo,iherb,grass,timber,ffm,adfm,wind,fload,df)
      
      
!     array declarations for a,b,c,d
      real,dimension(4):: a=(/-0.185900,-0.85900,-0.059660,-0.077373/)
      real,dimension(4):: b=(/30.0,19.2,13.8,22.5/)
      real,dimension(3):: c=(/4.5,12.5,27.5/)
      real,dimension(6):: d=(/16.0,10.0,7.0,5.0,4.0,3.0/)
      
!     this variable is used for calculating the difference between wet and dry bulb tmprtr      
      real :: dif 
      
!     loop counter     
      integer :: i 
      
!     ffm= 99.
!      adfm= 99.
!      df=0.
!      fload=0.
      
      if(isnow==1) then
          grass=0
          timber=0
          if(precip>0.1) then
              buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp( -1.175*(precip-.1)))
          endif
          if(buo<1) then
              buo=0
          end if
      endif

!     if there is no snow we calculate the ffm and df     
      if(isnow==0) then
          dif=dry-wet
          do i=1,3
             if(dif-c(i)<1) then
                 ffm=b(i)*EXP(a(i)*dif)
             else
                 ffm=b(4)*EXP(a(4)*dif)
             endif
          end do
          
          do i=1,6
              if(ffm-d(i)>0) then
                  df=i-1
              endif
          end do
          if(ffm<=1) then
              ffm=1
          else if(ffm>=1) then
              ffm = ffm + ( iherb-1 ) * 5.
          end if
          
          if(precip>=0.1) then
              buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp(-1.175*(precip-.1)))
              if(buo<0) then
                  buo=0
              endif
          else if(precip<=0.1) then
              buo=buo+df
          end if
          
          adfm = .9*ffm +.5 +9.5*exp(-buo/50.)

          if(adfm>30.) then
              if(ffm>30.) then
                  grass=1
                  timber=1
              else
                  if(wind>14) then
                      timber = .00918*(wind+14.) * (33.-adfm)**1.65 - 3.
                      grass = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.
                  else if(wind<14) then
                      timber = .01312*(wind+6.) * (33.-adfm)**1.65 - 3.
                      grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.
                  endif
              endif
          endif
        
          if(timber>0) then
              if(buo>0) then
                  fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
              endif
          end if
          
!         output
 
          write (*,*) 'Fine Fuel Moisture:', ffm
          write (*,*) 'Adjusted Fuel Moisture:', adfm
          write (*,*) 'Fine Fuel Spread:'
          write (*,*) 'Timber Spread Index:' , timber
          write (*,*) 'Fire Load Index:' , fload
          write (*,*) 'Build Up Index:' , buo
      endif ! the algorithm that takes place if there is no snow

      end

      
     
     
