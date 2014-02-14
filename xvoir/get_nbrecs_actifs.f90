!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2014  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
      subroutine get_nbrecs_actifs(nbrecs, iun) ! find number of valid records in a RPN standard file
      implicit none
      integer nbrecs, iun, key, fstinf, fstsui, ni,nj,nk
      
      nbrecs = 0 
      key = fstinf(iun, ni, nj, nk,  -1, ' ', -1, -1, -1, ' ', ' ')
      do while (key >= 0)  ! count valid records
        nbrecs = nbrecs+1
        key = fstsui(iun,ni,nj,nk)
      end do

      
      return
      end subroutine get_nbrecs_actifs
