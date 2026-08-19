module Auto_Diff
    implicit none

    type :: Number
        double precision :: primal
        double precision :: tangent
    contains
        ! Overloads the intrinsic operators to custom operators
        procedure :: add
        procedure :: sub
        procedure :: mul
        procedure :: div
        ! procedure :: pow
        generic :: operator(+) => add
        generic :: operator(-) => sub
        generic :: operator(*) => mul
        generic :: operator(/) => div
        ! generic :: operator(**)=> pow
    end type Number

        ! Overload the intrinsic names at module scope
    interface cos
        module procedure cos_dual
    end interface cos

    interface sin
        module procedure sin_dual
    end interface sin

    interface exp
        module procedure exp_dual
    end interface exp

    interface log
        module procedure log_dual
    end interface log

contains

    function add(self, other) result(sum_obj)
        class(Number), intent(in) :: self
        class(Number), intent(in) :: other
        type(Number) :: sum_obj
        
        ! 'self' represents the left operand, 'other' represents the right operand
        sum_obj%primal = self%primal + other%primal
        sum_obj%tangent = self%tangent + other%tangent
    end function add
    
    function sub(self, other) result(subtract_obj)
        class(Number), intent(in) :: self
        class(Number), intent(in) :: other
        type(Number) :: subtract_obj
        
        ! 'self' represents the left operand, 'other' represents the right operand
        subtract_obj%primal = self%primal - other%primal
        subtract_obj%tangent = self%tangent - other%tangent
    end function sub
    
    function mul(self, other) result(product_obj)
        class(Number), intent(in) :: self
        class(Number), intent(in) :: other
        type(Number) :: product_obj
        
        ! 'self' represents the left operand, 'other' represents the right operand
        product_obj%primal = self%primal * other%primal
        product_obj%tangent = self%primal * other%tangent + self%tangent * other%primal
    end function mul
    
    function div(self, other) result(divide_obj)
        class(Number), intent(in) :: self
        class(Number), intent(in) :: other
        type(Number) :: divide_obj
        
        ! 'self' represents the left operand, 'other' represents the right operand
        divide_obj%primal = self%primal / other%primal
        divide_obj%tangent = (self%tangent * other%primal - other%tangent * self%primal) / (other%primal)**2
    end function div

    ! function pow(self, n) result(power_obj)
    !     class(Number), intent(in) :: self
    !     class(Number), intent(in) :: n
    !     type(Number) :: power_obj
    !     power_obj%primal = exp(self%primal * log(n%primal))     ! x**n = exp(x*ln(n))
    !     power_obj%tangent = exp(self%primal * log(n%primal)) * (self%tangent * log(n%primal))
    ! end function pow

    function cos_dual(self) result(cos_obj)
        class(Number), intent(in) :: self
        type(Number) :: cos_obj
        cos_obj%primal = cos(self%primal)
        cos_obj%tangent = -sin(self%primal) * self%tangent
    end function cos_dual

    function sin_dual(self) result(sin_obj)
        class(Number), intent(in) :: self
        type(Number) :: sin_obj
        sin_obj%primal = sin(self%primal)
        sin_obj%tangent = cos(self%primal) * self%tangent
    end function sin_dual

    function exp_dual(self) result(exp_obj)
        class(Number), intent(in) :: self
        type(Number) :: exp_obj
        exp_obj%primal = exp(self%primal)
        exp_obj%tangent = exp(self%primal) * self%tangent
    end function exp_dual

    function log_dual(self) result(log_obj)
        class(Number), intent(in) :: self
        type(Number) :: log_obj
        log_obj%primal = log(self%primal)
        log_obj%tangent = self%tangent/self%primal
    end function log_dual

end module Auto_Diff

program main
    use Auto_Diff
    type(Number) :: x, y
    x = Number(2, 1)
    y = exp(x) + x*cos(x)*sin(x)
    print *, "Primal value  =", y%primal, "Tangent value  =", y%tangent
end program main