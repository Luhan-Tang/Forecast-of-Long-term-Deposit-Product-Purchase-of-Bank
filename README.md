# Prediction-of-long-term-deposit-product-purchases-by-bank-customers

本报告基于 Banking Dataset Classification 数据集，以年龄、工作、婚姻、教育、默认、住房、贷款、接触、月、一天的一周、期间、活动、结果 13 个变量为自变量，对银行客户是否会（是/否）订购该银行的定期存款产品进行预测，属于分类问题。首先，针对数据集进行了预先处理与初步分析，随后运用 Logistic 回归、线性判别分析、KNN 分类器与决策树 4 种不同的模型建模预测。其中采用 CV 准则对 KNN 模型的 K 值进行了选择，并对决策树进行了剪枝处理。最后，所有模型以精确率、召回率、特异度、ROC 曲线和 AUC值为评价指标进行了比较与分析。结果显示，Logistic 回归、线性判别分析与决策树模型的 AUC 值分别为：0.8547613、0.8589598、0.8425568，有很好的预测效果，且在精确率、召回率方面，这三个模型均高于 91%，预测效果显著优于 KNN 分类器模型。本报告通过比较、评估不同模型效果，提高了预测结果的精准度，同时也为银行利用模型解决客户购买问题提供了不同的决策思路。

This report is based on the Banking Dataset Classification dataset, which comprises 13 variables, including age, work, marriage, education, default, housing, loan, contact, month, day of the week, period, activity, and result, all considered as independent variables. The objective is to predict whether a bank customer will (yes/no) subscribe to the bank's fixed deposit products, making it a classification task.

Initially, the dataset underwent preprocessing and preliminary analysis. Subsequently, four different models, namely Logistic Regression, Linear Discriminant Analysis, K-Nearest Neighbors (KNN) classifier, and Decision Tree, were employed for modeling and prediction. The optimal K value for the KNN model was determined using the cross-validation criterion, and the Decision Tree model was pruned.

In the final step, all models were compared and evaluated using metrics such as accuracy, recall, specificity, ROC curve, and AUC value. The results indicate that the AUC values for the Logistic Regression, Linear Discriminant Analysis, and Decision Tree models are 0.8547613, 0.8589598, and 0.8425568, respectively, demonstrating strong predictive performance. Moreover, these three models achieved accuracy and recall rates exceeding 91%, surpassing the predictive performance of the KNN classifier model.

Through the comparison and evaluation of various models, this report not only enhances the accuracy of forecasting outcomes but also offers different decision-making insights for banks in addressing customer subscription issues through the application of these models.
