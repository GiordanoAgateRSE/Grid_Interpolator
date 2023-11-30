!-------------------------------------------------------------------------------
! "Grid_Interpolator v.3.0" (Grid manager tool)
! Copyright 2016-2022 (RSE SpA)
! "Grid_Interpolator v.3.0" authors and email contact are provided on the 
! documentation file.
! This file is part of Grid_Interpolator v.3.0 .
! Grid_Interpolator v.3.0 is free software: you can redistribute it and/or 
! modify it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! Grid_Interpolator is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with Grid_Interpolator. If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! Program unit: interpolation
! Description: interpolation of the input data into the output grid
!-------------------------------------------------------------------------------
subroutine interpolation
!------------------------
! Modules
!------------------------
use main_module
!------------------------
! Declarations
!------------------------
implicit none
! Generic node indices along x/y/z axis within "do" constructs
integer(4) :: ix,iy,iz
! Globlal ID of a generic input record
integer(4) :: i_rec
! Percentage of rows interpolated
integer(4) :: iy_perc_done
integer(4) :: aux_integer,iy_count
double precision :: denom,distance,eps
character(100) :: array_name
!------------------------
! Explicit interfaces
!------------------------
interface
   subroutine allocate_de_dp_r2(allocate_flag,array,extent_1,extent_2,uerr,    &
                                array_name)
      implicit none
      double precision,dimension(:,:),allocatable,intent(inout) :: array
      logical,intent(in) :: allocate_flag
      integer(4),intent(in) :: extent_1
      integer(4),intent(in) :: extent_2
      integer(4),intent(in) :: uerr
      character(100),intent(in) :: array_name
   end subroutine allocate_de_dp_r2
   subroutine allocate_de_dp_r4(allocate_flag,array,extent_1,extent_2,         &
                                extent_3,extent_4,uerr,array_name)
      implicit none
      double precision,dimension(:,:,:,:),allocatable,intent(inout) :: array
      logical,intent(in) :: allocate_flag
      integer(4),intent(in) :: extent_1
      integer(4),intent(in) :: extent_2
      integer(4),intent(in) :: extent_3
      integer(4),intent(in) :: extent_4
      integer(4),intent(in) :: uerr
      character(100),intent(in) :: array_name
   end subroutine allocate_de_dp_r4
   subroutine delta_lon_lat_to_delta_x_y(delta_lon,delta_lat,abs_mean_latitude,&
                                         delta_x,delta_y)
      implicit none
      double precision,intent(in) :: delta_lon,delta_lat,abs_mean_latitude
      double precision,intent(out) :: delta_x,delta_y
   end subroutine delta_lon_lat_to_delta_x_y
end interface
!------------------------
! Allocations
!------------------------
array_name = "field_out"
call allocate_de_dp_r4(.true.,field_out,nx_out+1,ny_out+1,nz_out+1,4,uerr,     &
                       array_name)
!------------------------
! Initializations
!------------------------
delta_lon = 1.d0
delta_lat = 1.d0
field_out(:,:,:,:) = 0.d0
aux_integer = 0
iy_count = 0
!------------------------
! Statements
!------------------------
do iz=1,nz_out+1
!$omp parallel do default(none)                                                &
!$omp shared(n_points_in,field_in,field_out,dx_out,threshold_pos,nx_out,ny_out)&
!$omp shared(threshold_neg,normalized_influence_radius,distance_exponent)      &
!$omp shared(missing_data_value,iz,x_min_out,y_min_out,z_min_out,dy_out,dz_out)&
!$omp shared(aux_integer,iy_count,iy_perc_done)                                &
!$omp private(ix,iy,denom,distance,i_rec,eps)
   do iy=1,ny_out+1
      do ix=1,nx_out+1
! Output data are written on the vertices of the grid cells, not on the cell 
! barycentres. This choice is mandatory to keep shared the boundaries of 
! adjacent output files, in case of multiple output file related to multiple 
! spatial resolution. 
         field_out(ix,iy,iz,1) = x_min_out + dx_out * (ix - 1)
         field_out(ix,iy,iz,2) = y_min_out + dy_out * (iy - 1)
         field_out(ix,iy,iz,3) = z_min_out + dz_out * (iz - 1)
         denom = 0.d0
         do i_rec=1,n_points_in
            distance = dsqrt((field_in(i_rec,1) - field_out(ix,iy,iz,1)) **    &
                       2 + (field_in(i_rec,2) - field_out(ix,iy,iz,2)) ** 2    &
                       + (field_in(i_rec,3) - field_out(ix,iy,iz,3)) ** 2)
            if (distance>(normalized_influence_radius*dx_out*dsqrt(3.d0))) cycle
            eps = 1.d-9 * (-999.d0 ** 2) / dabs(-999.d0)
            if ((field_in(i_rec,4)<=(-999.d0+eps)).and.                        &
               (field_in(i_rec,4)>=(-999.d0-eps))) cycle
            if ((field_in(i_rec,4)>threshold_pos).or.                          &
               (field_in(i_rec,4)<threshold_neg)) cycle
            if (distance>=(1.d-6*dx_out)) then
               field_out(ix,iy,iz,4) = field_out(ix,iy,iz,4) +                 &
                                       field_in(i_rec,4) / distance **         &
                                       abs(distance_exponent)
               denom = denom + 1.d0 / distance ** abs(distance_exponent)
               else
                  field_out(ix,iy,iz,4) = field_in(i_rec,4)
                  denom = 1.d0
                  exit
            endif
         enddo
         if (denom/=0.d0) then
            field_out(ix,iy,iz,4) = field_out(ix,iy,iz,4) / denom
            else
               field_out(ix,iy,iz,4) = missing_data_value
         endif
      enddo
!$omp critical (omp_interpolation_progress)
      iy_count = iy_count + 1
      iy_perc_done = int(dfloat(iy_count) / dfloat(ny_out+1) * 100.d0)
      if (iy_perc_done>=aux_integer) then
         write(*,'(a,i9,a,i3,a)') "      iz: ",iz,", iy=",iy_perc_done,        &
            "%(ny_out+1)"
         aux_integer = aux_integer + 10
      endif
!$omp end critical (omp_interpolation_progress)
   enddo
!$omp end parallel do
enddo
if (abs_mean_latitude>-1.d-9) then
! Reference system conversion: cartographic to geographic: (X,Y) in (m) to 
! (lon,lat) in (°)
   call delta_lon_lat_to_delta_x_y(delta_lon,delta_lat,abs_mean_latitude,      &
      delta_x,delta_y)
   field_out(:,:,:,1) = delta_lon * field_out(:,:,:,1) / delta_x
   field_out(:,:,:,2) = delta_lat * field_out(:,:,:,2) / delta_y
endif
! Reference system conversion: local (SPHERA) to global (georeferenced)
field_out(:,:,:,1) = field_out(:,:,:,1) + x_lon_trans
field_out(:,:,:,2) = field_out(:,:,:,2) + y_lat_trans
!------------------------
! Deallocations
!------------------------
array_name = "field_in"
call allocate_de_dp_r2(.false.,field_in,0,0,uerr,array_name)
return
end subroutine interpolation
