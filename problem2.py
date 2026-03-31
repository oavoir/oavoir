import pandas as pd

# 문제 2: P 자동차 회사의 차종별 데이터

# 1) 데이터프레임 생성
data = {
    'name': ['A', 'B', 'C', 'D', 'E', 'F', 'G'],
    'horse power': [130, 250, 190, 300, 210, 220, 170],
    'weight': [1.9, 2.6, 2.2, 2.9, 2.4, 2.3, 2.2],
    'efficiency': [16.3, 10.2, 11.1, 7.1, 12.1, 13.2, 14.2]
}
df = pd.DataFrame(data)
print("1) 데이터프레임 생성:")
print(df)

# 2) name 열을 인덱스로 지정
df = df.set_index('name')
print("\n2) name을 인덱스로 지정:")
print(df)

# 3) 마력 * 연비가 가장 큰 차종 찾기
df['score'] = df['horse power'] * df['efficiency']
best = df['score'].idxmax()
print(f"\n3) 마력 × 연비가 가장 큰 차종: {best} (값: {df.loc[best, 'score']})")
