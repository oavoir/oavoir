import numpy as np

# 문제 3: 브로드캐스팅

arr = np.array([[1, 2, 3, 4],
                [5, 6, 7, 8],
                [9, 10, 11, 12]])
print("원본 배열:")
print(arr)

# 1) 스칼라 값 10을 곱하여 브로드캐스팅
arr = arr * 10
print("\n1) 스칼라 10 곱한 결과:")
print(arr)

# 2) 1차원 배열 [1,2,3,4]를 더하여 행별 덧셈 (브로드캐스팅)
add_row = np.array([1, 2, 3, 4])
result2 = arr + add_row
print("\n2) [1,2,3,4] 행별 브로드캐스팅 덧셈 결과:")
print(result2)

# 3) 3x1 배열 [[10],[20],[30]]을 더하여 열 방향 브로드캐스팅
add_col = np.array([[10], [20], [30]])
result3 = arr + add_col
print("\n3) [[10],[20],[30]] 열 방향 브로드캐스팅 덧셈 결과:")
print(result3)
