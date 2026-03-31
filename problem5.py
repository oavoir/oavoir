import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from skimage.io import imread
from skimage.transform import resize
from sklearn.svm import LinearSVC
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix

# 문제 5: 이미지 분류

url = 'https://github.com/dknife/ML2nd/raw/main/data/Proj1/40/'
images = []
for i in range(1, 41):  # img01 ~ img40
    file = f'{url}img{i:02d}.jpg'
    try:
        img = imread(file)
        img = resize(img, (24, 24, 3), anti_aliasing=True)
        images.append(img)
    except Exception as e:
        print(f"Error loading img{i:02d}.jpg: {e}")

print(f"Total images loaded: {len(images)}")
images = np.array(images)
print("Final shape:", images.shape)  # (40, 24, 24, 3)


def plot_images(nRow, nCol, imgs, filename=None):
    fig, axes = plt.subplots(nRow, nCol, figsize=(nCol, nRow))
    axes = axes.flatten() if nRow > 1 else axes

    for i in range(nRow * nCol):
        if i < len(imgs):
            axes[i].imshow(imgs[i])
        axes[i].axis('off')

    plt.tight_layout()
    if filename:
        plt.savefig(filename)
    plt.close()


plot_images(4, 10, images, 'problem5_all_images.png')
print("전체 데이터 출력 완료")

# 1) 훈련 데이터 X (처음 30장), 테스트 데이터 X_test (나머지 8장, image 34, 38 제외)
X = images[:30]
# image 34 = index 33, image 38 = index 37 → 제외
test_indices = [i for i in range(30, 40) if i not in [33, 37]]
X_test = images[test_indices]

print('\n훈련용 데이터')
plot_images(3, 10, X, 'problem5_train.png')
print('테스트 데이터')
plot_images(1, 8, X_test, 'problem5_test.png')

# 정답 label 생성 (0: animal, 1: human)
y_all = np.array([
    # img01~img10
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    # img11~img20
    0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    # img21~img30
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    # test data
    1, 0, 1, 1, 0, 1, 1, 0
])

# 2) SVM 학습 및 예측

# 훈련/테스트 레이블 분리
y_train = y_all[:30]
y_test = y_all[30:]

# 이미지를 1차원 벡터로 변환
X_train_flat = X.reshape(X.shape[0], -1)
X_test_flat = X_test.reshape(X_test.shape[0], -1)

# feature scaling
scaler = StandardScaler()
X_train_scaled = scaler.fit_transform(X_train_flat)
X_test_scaled = scaler.transform(X_test_flat)

# SVM 모델 생성 및 학습
model = LinearSVC()
model.fit(X_train_scaled, y_train)

# 예측
y_pred = model.predict(X_test_scaled)

print("예측 결과:", y_pred)
print("실제 정답:", y_test)

# 분류 리포트 작성
print("\n분류 리포트:")
print(classification_report(y_test, y_pred))
