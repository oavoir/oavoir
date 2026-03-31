import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# 문제 4: vehicle_prod.csv 파일 분석

# 1) CSV 파일 읽기, 국가명을 인덱스로 설정, 첫 3개 행 출력
url = 'https://github.com/dknife/ML2nd/raw/main/data/vehicle_prod.csv'
df = pd.read_csv(url, index_col=0)
print("1) 첫 3개 행:")
print(df.head(3))

# 2) 2010년도 열을 막대 그래프로 시각화
plt.figure(figsize=(10, 6))
df['2010'].plot(kind='bar')
plt.title('2010 Vehicle Production by Country')
plt.ylabel('Production')
plt.tight_layout()
plt.savefig('problem4_2010_bar.png')
plt.close()
print("\n2) 2010년도 막대 그래프 저장 완료 (problem4_2010_bar.png)")

# 3) 각 연도별 기초 통계량
print("\n3) 연도별 기초 통계량 (평균, 표준편차, 최솟값, 최댓값):")
stats = df.describe().loc[['mean', 'std', 'min', 'max']]
print(stats)

# 4) 모든 연도의 평균 생산 대수를 계산해 average 열 추가
df['average'] = df.mean(axis=1)
print("\n4) average 열 추가 결과:")
print(df)
