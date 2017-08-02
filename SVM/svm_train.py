# coding:UTF-8
import numpy as np
import SVM
def load_data_libsvm(data_file):
    '''导入训练数据
      input: data_file(string) : 训练数据所在的文件
      output: data(mat):训练样本的特征
      label(mat): 训练样本的标签
    '''
    data = []
    label = []
    f = open(data_file)
    for line in f:
        lines = line.strip().split(' ')
        #提取得到label
        label.append(float(lines[0]))
        #提取出特征,将其放入到矩阵中
        print(label)
        index = 0
        tmp = []
        for i in range(1,len(lines)):
            li = lines[i].strip().split(":")
            if int(li[0]) - 1 == index:
                tmp.append(float(li[1]))
            else:
                while(int(li[0]) - 1 > index):
                    tmp.append(0)
                    index += 1
                tmp.append(float(li[1]))
            index += 1
        while len(tmp) < 13:
            tmp.append(0)
        data.append(tmp)
        f.close()
        return np.mat(data) , np.mat(label).T


if __name__ == "__main__":
    # 导入训练数据
    print("-----------load data-----------")
    dataSet , labels = load_data_libsvm("heart_scale")
    #训练SVM模型
    print("-----------training-----------")
    C = 0.6
    toler = 0.001
    maxIter = 500
    svm = SVM.SVM(dataSet, labels, C, toler,('rbf',0.431029))
    svm_model =svm.SVM_training(dataSet, labels, C, toler, maxIter)
    #计算训练的准确性
    print("-----------cal accuracy-----------")
    accuracy = svm.cal_accuracy(svm_model, dataSet, labels)

    #保存最终的svm模型
    print("-----------save model-----------")
    svm.save_svm_model(svm_model, "model_file")

