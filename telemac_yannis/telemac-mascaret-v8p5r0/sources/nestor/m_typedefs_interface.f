!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      MODULE      m_TypeDefs_InterFace           !******       SUBROUTINE  ********************
!**                                               *********************************************
!**                                               *********************************************
!
      INTEGER, PARAMETER, PRIVATE :: R8 = SELECTED_REAL_KIND( 10, 60 ) ! DOUBLE PRECISION SIZE
      PUBLIC
!
      TYPE  :: t_PointerToArrayOfReals
        REAL (KIND=R8), POINTER, DIMENSION (:) :: R
      END TYPE t_PointerToArrayOfReals
!
!
!***                                              ********************************************
!***                                              ********************************************
      END MODULE  m_TypeDefs_InterFace           !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************