# 道路路網權重探索

## 原由
金門縣缺乏交通量資料，在執行如道路規劃等工作時缺乏客觀量化資料。因此為了從道路路網架構上，試著去衡量道路的重要性及彼此間的關係，所以採用了隨機模擬的方式。

## 方法
使用 Google Maps API 進行10000次隨機交通模擬，並從模擬結果中取出路名，以網絡分析方式量化道路間的關係。

## 步驟
1. 使用QGIS產生隨機點，隨機點分佈比例採金門縣各村里的人口數及門牌數的平均值。製作kinmen_randomPoint.csv檔，刪除位於小島（如建功嶼）的點位後，實際可用19987點。
2. 隨機從kinmen_randomPoint.csv取得2點，若2點間直線距離大於1公里，則使用 Google Maps API 執行路徑導航。重覆執行至取得1萬筆資料，R 程式碼檔為 RandomTripV3.R。
3. 清理資料，從導航結果中取出路名，並建立路到路的關係。R 程式碼檔為 RoadNetwork.R。
4. 進行網絡分析並視覺化。R 程式碼檔為 RoadNetworkAnalysis.R。

## 結果
![網絡圖](https://github.com/chiahuaw/RoadNetwork/blob/master/result_plot.png)
只顯示前20%道路的路名。nodes 119 個，edges 有 378 個。