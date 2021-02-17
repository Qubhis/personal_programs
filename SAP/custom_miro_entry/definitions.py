class MiroMainScreenError(Exception):
    """Raised when a modal window with company code is not found"""

    pass


class InsertedPoLinesDoNotMatch(Exception):
    """Raised when inserted purchase order lines doesn't match what we've
    expected"""

    pass


class BalanceNotZero(Exception):
    """Raised when final Balance is not zero"""

    pass


class PoLineCountMismatch(Exception):
    """Raised when count of inserted lines is not same as the count on the
    screen"""

    pass


class UnexpectedError(Exception):
    """Used whenever something unexpected has happended"""

    pass


class TooManyGLAccounts(Exception):
    """Raised when there are more than expected count of G/L accounts in simulation"""

    pass


if __name__ == "__main__":
    pass