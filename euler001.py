#!/user/bin/env python3

def solution(cap: int) -> int:
    """The sum of all multiples of 3 or five less than cap"""
    return sum(i for i in range(cap) if i % 3 == 0 or i % 5 == 0)

if __name__ == '__main__':
    assert solution(10) == 23, "Problem example failed"
    print(solution(1000))
    
