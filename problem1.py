import numpy as np

# 1) 난수로 이루어진 10000x10000 정수 배열 생성 (0~9)
arr = np.random.randint(0, 10, size=(10000, 10000))
print("1) 배열 생성 완료")
print("shape:", arr.shape)
print("첫 5x5:\n", arr[:5, :5])

# 2) 모든 요소를 1 증가
arr = arr + 1
print("\n2) 모든 요소 +1")
print("첫 5x5:\n", arr[:5, :5])

# 3) 짝수번째 행을 모두 0으로 만들기
#    물리적 위치 기준: 2, 4, 6, ... → Python index: 1, 3, 5, ...
arr[1::2, :] = 0
print("\n3) 짝수번째 행(물리적 위치 2,4,6,...) = 0")
print("첫 5x5:\n", arr[:5, :5])

# 4) 홀수번째 열을 모두 0으로 만들기
#    물리적 위치 기준: 1, 3, 5, ... → Python index: 0, 2, 4, ...
arr[:, ::2] = 0
print("\n4) 홀수번째 열(물리적 위치 1,3,5,...) = 0")
print("첫 5x5:\n", arr[:5, :5])
