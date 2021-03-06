# 第Ｏ部份：環境設定 --------------------------------
if (!("benchmarkme" %in% rownames(installed.packages()))) try(install.packages("benchmarkme"))
t_sessioninfo_running<-gsub("[>=()]","",gsub(" ","",sessionInfo()$running))
t_sessioninfo_running_with_cpu<-try(paste0(t_sessioninfo_running,benchmarkme::get_cpu()$model))
t_sessioninfo_running_with_cpu_locale<-try(gsub(pattern=" ",replacement = "", x=paste0(t_sessioninfo_running_with_cpu,unlist(strsplit(unlist(strsplit(sessionInfo()$locale,split=";"))[1], split="="))[2])))
source_sharedfuncs_r_path<-try(here::here())
if(is(source_sharedfuncs_r_path, 'try-error')) source_sharedfuncs_r_path<-"."
source(file = paste0(source_sharedfuncs_r_path,"/shared_functions.R"), encoding="UTF-8")

#選舉資料
term_to_survey <- data.frame("term"=c(5,6,7,7,8,8,9), "SURVEY"=c("2004citizen","2004citizen","2010env","2010overall","2010env","2010overall","2016citizen"))
gc(verbose=TRUE)

survey_imputation_and_measurement<-custom_ret_survey_imputation_and_measurement(paths_to_survey_imputation_and_measurement_file)
survey_codebook<-openxlsx::read.xlsx(paste0(dataset_file_directory,"all_survey_questions_englished.xlsx"),sheet = 4)


# 問卷使用說明 --------------------------------
if (FALSE) {
  readme="
SURVEY PACKAGE要用
weight(column)
population size(column) finite population correction
staging information
====================2004 citizen====================
抽樣說明
一、抽樣原則
本計畫是以台灣地區年滿18 歲及以上之中華民國國民（74 年12 月31 日以前出生者）
為研究母體，並以台灣地區戶籍資料檔為抽樣名冊（sampling frame），利用分層等機率三
階段抽樣法（probability proportional to size，PPS）抽出受訪對象。實際抽樣執行程
序如下：利用羅啟宏先生所撰著之「台灣省鄉鎮發展類型之研究」，依據人口特性、商業發
展、公共設施、財政狀況、地理環境等相關指標，將台灣各鄉鎮分為七層，再加上台北市、
高雄市、省轄市各為一層，共計十層。在抽樣時，先計算各分層所有鄉鎮之人口數，依其
人口數比例來分配各分層欲抽出之人數，並在各分層中依人口數多寡而抽取一定數目的鄉
鎮市區；其後，在每一鄉鎮市區中，再依人口數之多寡依照等距抽樣法（systematic
sampling）有系統地抽取一定數目的村里；最後，在前述中選村里中再同樣依等距抽樣法
抽取一定數目的受訪個案。
1.根據內政部臺灣地區人口統計資料計算出臺灣各鄉鎮市區在調查當年十八（含）歲以上
的人口數（本次調查中，兩主題問卷的樣本年齡不設上限）。
2.將臺灣全省各鄉鎮市依其發展特性歸類成新興鄉鎮、山地鄉鎮、綜合性市鎮、坡地鄉鎮、
偏遠鄉鎮與服務鄉鎮等七類，再加上臺北市、高雄市和省轄市，合計十個等級。
3.匯集各鄉鎮市區人口資料以統計出各等級區域十八（含）歲以上的人口總數。
4.計算各等級地區十八（含）歲以上人口數的比例。
5.按照各等級地區十八（含）歲以上人口比例，算出各等級地區所應抽出之樣本數。
6.依各等級分別進行獨立抽樣，以鄉鎮市區為第一抽出單位 （ primary selection unit,
PSU ），以村里為第二抽出單位，個人為最後抽出單位。唯為使臺北市和高雄市之調查
地區平均分布，將不以「區」為第一抽出單位，乃打破區界直接以里為第一抽出單位。
7.各階段各單位的抽樣，採抽取率與單位大小成比率 （ PPS ） 方式來決定。
8.依據以上的抽樣原則，抽出台北市、高雄市、省轄市及其他鄉鎮的若干村里做調查。
二、樣本數額與樣本名單抽樣程序
研究問卷Ι─公民權組問卷因保留部份三期四次題目，所以這次調查不做電話預試，
只做面訪預試；研究問卷Ⅱ─宗教文化組因為重新設計的問卷，為求設計上的嚴謹與周密，
除進行新題組的電話預試外，也透過面訪預試，來找出問卷題目的疏失，以及預估正式調
查的可行性和成功性。面訪預試時，在台北縣市、彰化縣、雲林縣、高雄縣市地區隨機抽
樣，期望成功數為兩組各200 人，膨脹樣本數為公民權組490 人、宗教文化組490 人，實
際訪問結果為公民權組成功樣本數169 人、宗教文化組155 人。本計畫預計完成樣本數為
4024 案（公民權組2012 案，宗教組2012 案），為了避免其他因素干擾（如拒訪等）而致
無法達成預計完成數，本計畫抽樣乃是採取膨脹方式來估算每一村里所需抽取之人數；而
在進行抽樣時，所參照之膨脹比例，其中台北市及高雄市是依據「台灣地區社會變遷基本
調查第四期第三次」調查計畫中之該層完成率來估算膨脹比例，台北市膨脹比例2.3 倍，
高雄市膨脹比例為1.9 倍，而除了台北市及高雄市之外的各層，膨脹比例則是依據「台灣
地區社會變遷基本調查第四期第三次」與「台灣地區社會變遷基本調查第四期第四次」調
查計畫中每一分層之完成率的平均來估算膨脹比例，膨脹比例約介於1.6 倍至2.3 倍之間，
其中，公民權組共計抽出4012 案，而宗教組共計抽出3955 案。
抽樣工作分預試與正式調查兩次進行。抽樣程序如下：
A．鄉鎮市（區）的抽樣：
1.根據行政院內政部「民國九十二年臺閩地區人口統計秋季季刊」查閱所有鄉鎮（區）別
及各鄉鎮市（區）的人口數。
2.再依據羅啟宏先生所著之「台灣省鄉鎮發展類型之研究」的分層原則（工商市鎮、新興
鄉鎮、綜合性市鎮、服務性鄉鎮、坡地鄉鎮、偏遠鄉鎮、山地鄉鎮），並加上台北市、高
雄市、省轄市，共分為十個層級。將同分層之鄉鎮市（區）按人口數由小到大排列，並
做人口數的累加。
3.以該層的總人口數T 除以所需的鄉鎮市（區）數ｎ，Ｔ／ｎ＝Ｋ（Ｋ四捨五入，留下整
數部分）。
4.在１～Ｋ中取一個亂數R，則R 在累加人口數該欄的落點所在即是抽取的第一個鄉鎮市
（區）。
5.然後以R 為基礎，再往下找第R＋K 的落點，以此類推，直到抽滿所需的鄉鎮市（區）數
為止。
B．村里的抽樣
1.根據行政院內政部民國九十二年年終靜態檔。
2.依照其排列各村里的原次序做人口數的累加。
3.以該鄉鎮市區的總人口數Ｔ除以所需的村里數ｎ，Ｔ／ｎ＝Ｋ（Ｋ四捨五入，留下整數
部分）。
4.在１～Ｋ中取一個亂數Ｒ，則Ｒ的落點所在即是抽出的第一個村里。
5.然後以Ｒ為基礎，再往下找到第Ｒ┼Ｋ的落點，以此類推，直到抽滿所需的村里數。
C．選取合格者
1.樣本合格者條件：18 歲以上之中華民國國民。
2.程序：
（1）查該村里十八歲以上人數Ｈ。
（2）以該村里十八歲以上人數除以所需樣本數ｎ
Ｈ／ｎ＝Ｂ
Ｂ可能是一個帶有小數點的數字如42.31 或53.82…
Ｂ去掉小數點後的數字，留下整數部分（42 或53）Ｂ＊。
（3）從１～Ｂ＊中抽出一個亂數Ｒ，若第Ｒ人為合格者，則為抽中之第一人。
注意：若Ｒ為不合格者，請找Ｒ人的前一人（即Ｒ－１人）為合格者。若仍為不合格者，
則找後一人（即為Ｒ＋１），以此法類推，Ｒ－２，Ｒ＋２……到Ｒ＋３人仍找不
到則放棄，然後以Ｒ為基礎再往下找到Ｒ＋Ｂ＊人。若找到合格者之後，亦以Ｒ
＋Ｂ＊人為基礎往後找。


貳、加權說明
一、加權母體說明
利用92 年抽樣的母體資料進行加權，選取18 歲以上人口，並扣除外島人口，
包括：連江縣、金門縣、屏東縣琉球鄉、台東縣綠島鄉、台東縣蘭嶼鄉、澎湖縣，
分別計算母體性別、年齡及地區層別的分佈。
在教育程度的母體參數部份，使用民國79 年及民國89 年戶口普查資料進行
推估，其方式為先扣除外籍人士（民國89 年的戶口普查資料，利用「國籍代碼」
變項挑出“本國人＂(000) 資料；民國79 年的戶口普查資料，利用「籍別」變
項，挑出“台閩地區＂及“他省市＂者），再將年齡限制為18 歲以上，並扣除上
述外島人口。最後，假設十年之間教育程度每年成長（或衰減）率固定，推算而
得民國92 年母體的教育程度分佈。加權之變項、分組方式及母體參數如表一。

二、加權方式說明
為了解訪問成功的樣本結構是否與母體一致，故就「性別」、「年齡」、「教
育程度」及「地區層別」來檢定訪問成功的樣本是否具有代表性。
由表二，本研究之成功樣本代表性檢定顯示，成功樣本在「年齡」、「教育
程度」及「地區層別」上的結構明顯與母體不一致。為了與母體結構更符合，避
免造成分析資料時推論的偏差，故以「多變數反覆加權法(raking)」對成功樣本
進行加權。
由表三，經加權處理後，成功樣本在「性別」、「年齡」、「教育程度」及
「地區層別」的分佈上，均與母體分佈無差異。
====================2010 env====================
資料使用說明
(1)	複雜抽樣變項：根據計畫小組的抽樣架構，提供抽樣依據的地區分層(Stratum)、第一抽出單位(Primary Sampling Unit, PSU)、第二抽出單位(Secondary Sampling Unit, SSU)等複雜抽樣變項。
(2)	根據計畫小組的抽樣架構(PSU、SSU以及各村里抽取數)，以等比例放大抽取率方式新增不等機率抽樣權數(wsel)；根據計畫小組的抽樣架構與其抽樣母體資料，資料庫以Raking方式新增權值變項(wr)。
(3)	郵遞區號：依據訪問村里所在位置，提供郵遞區號資訊。

權值說明
由於調查之抽樣採取分層三階段pps抽樣法，及膨脹樣本無替換的設計，使得樣本的中選機率是不相等的，這部分將以調查設計的加權權數來處理。表2與表3分別為加權前與加權後之結果。
(一)	不等機率抽樣權值（unequal probabilities of selection, sampling weights）
由於本計畫採用分層三階段PPS抽樣，再加上使用膨脹樣本的策略，使每個人具有不同的中選機率。為了補償此不等機率抽樣的問題，在資料處理上將採不等機率加權的措施。首先，分層三階段PPS抽樣的抽取率計算方式如下
(二)	多變項反覆加權法的權值
為使成功樣本結構具有代表性並符合母體結構，在完成前述不等機率加權權值 後，隨即針對「性別」、「年齡」（六分類）、「教育程度」（五分類）與「地區層別」（六分類）四個變項進行樣本代表性檢定（卡方檢定），並採用「多變項反覆加權法」進行加權，直到成功樣本代表性檢定結果符合母體的分佈狀況為止。由表十三和表十四之結果顯示成功之樣本與母體資料均無顯著差異，則表示反覆加權過後成功樣本具有代表性。

抽樣說明
一、抽樣原則
本計畫是以台灣地區具有國籍，在民國80 年12 月31 日以前出生（18 歲以上）的民
眾為研究母體，並以台灣地區戶籍資料檔為抽樣名冊（sampling frame），利用分層等機率
三階段抽樣法（Stratified Three-Stage Probability Proportional to Size, PPS）抽出受訪對象。
實際抽樣執行程序如下：利用「人口密度1」、「教育程度2」、「65 歲以上人口百分比」、「15-64
歲人口百分比」、「工業人口百分比3」、「商業人口百分比4」等指標，將台灣之鄉鎮市區分為
七層。在抽樣時，先計算各分層所有鄉鎮市區之人口數，依其人口數比例來分配各分層欲
抽出之人數，並在各分層中依人口數多寡而抽取一定數目的鄉鎮市區；其後，在每一鄉鎮
市區中，再依人口數之多寡依照等距抽樣法（systematic sampling）有系統地抽取一定數目
的村里；最後，在前述中選村里中再同樣依等距抽樣法抽取一定數目的受訪個案。
1. 根據內政部臺灣地區人口統計資料計算出臺灣各鄉鎮市區在調查當年十八（含）歲以上
的人口數（本次調查中，兩主題問卷的樣本年齡不設上限）。
2. 將臺灣全省各鄉鎮市依其發展特性歸類成核心都市、一般都市、新興市鎮、傳統產業市
鎮、一般鄉鎮、高齡化鄉鎮、偏遠鄉鎮等七個集群，再將高齡化鄉鎮和偏遠鄉鎮合併為
一類，合計六個等級。
3. 匯集各鄉鎮市區人口資料以統計出各等級區域十八（含）歲以上的人口總數。
4. 計算各等級地區十八（含）歲以上人口數的比例。
5. 按照各等級地區十八（含）歲以上人口比例，算出各等級地區所應抽出之樣本數。
6. 依各等級分別進行獨立抽樣，以鄉鎮市區為第一抽出單位 （primary selection unit,
PSU），以村里為第二抽出單位，個人為最後抽出單位。
7. 各階段各單位的抽樣，採抽取率與單位大小成比率（PPS）方式來決定。
8. 依據以上的抽樣原則，抽出台北市、高雄市、省轄市下轄各區以及其他鄉鎮市的若干村
里做調查。
由於環境組的調查應問卷小組建議針對區域分佈（北中南東）中抽樣數較少的地區（花
東地區）進行樣本加抽，因此今年度綜合問卷組和環境組的抽樣設計稍有不同。實際抽樣
執行以台灣地區戶籍資料檔為抽樣名冊（sampling frame），利用分層等機率三階段抽樣法
（PPS）進行抽樣。
1. 綜合問卷組：
該組分層所依據的變項則包括人口及各種都市化程度指標，將台灣之鄉鎮市區分為七
個層級。1實際抽樣執行方式因第七層級之人口數較少，基於調查行政便利性的考量，故合
併至第六層級。使台灣之鄉鎮市區為六層級後，即依據內政部戶政司所提供民國99 年2 月
底人口統計檔計算每一層級的人口比例；再依所計算出的人口比例，計算各層級所需抽取
的鄉鎮市區數，確定各層級所需抽取鄉鎮市區數後再依各層級人口數的多寡以等距抽樣方
式（systematic sampling）抽出中選鄉鎮，其後，在每一鄉鎮市區中，再依人口數之多寡依
照等距抽樣法有系統地抽取兩個村里；最後再利用所選中村里依等距原則抽出膨脹後之受
訪對象。
2. 環境組：
該組以「北中南東」四地理區搭配綜合問卷組採用之六層級重新進行新分層，使該樣
本具有北中南東四區的獨立代表性。「北中南東」縣市分布如下：
北部：北北基桃竹、中部：苗中彰投雲、南部：嘉南高高屏澎、東部：宜花東。
北部地區：將1～2 層級合併、3~6 層級合併，劃分為新分層1、2。中部與南部1～2 層級合併；3~4 層級合併；5-6 層級合併，分別劃分為新分層3、4、5 及新分層6、7、8。東部
則是1~4 層級合併；5~6 層級合併，劃分為新分層9、10。將全國鄉鎮市區重新分為十層級，
再依據內政部戶政司所提供民國99 年2 月底人口統計檔計算每一層級的人口比例；並依所
計算出的人口比例計算各層級所需抽取的鄉鎮數，確定各層級所需抽取鄉鎮市區數後再依
各層級人口數的多寡以等距抽樣方式（systematic sampling）抽出中選鄉鎮。其後，在每一
鄉鎮市區中，再依人口數之多寡依照等距抽樣法有系統地抽取兩個村里；最後再利用所選
中村里依等距原則抽出膨脹後之受訪對象。
二、樣本數額與樣本名單抽樣程序
本年度調查研究問卷Ι─綜合組問卷和研究問卷Ⅱ─環境組問卷只做面訪預試。本計畫
預計完成樣本數為300 案（綜合組150 案，環境組150 案），為了避免其他因素干擾（如拒
訪等）而致無法達成預計完成數，本計畫抽樣乃是採取膨脹方式來估算每一村里所需抽取
之人數；而在進行抽樣時，所參照之膨脹比例則是依據「台灣地區社會變遷基本調查」第
五期第三次、第五期第四次、第五期第五次調查計畫中每一分層之完成率來估算膨脹比例，
膨脹比例約介於1.5 倍至3 倍之間。面訪預試調查自台北市、台北縣、基隆市、彰化縣、
雲林縣、台南縣、高雄市、高雄縣進行隨機抽樣，其中，綜合組共計抽出七個村里338 案，
而環境組亦抽出七個村里342 案。「綜合組」的期望成功樣本數為150 人，實際成功樣本數
則為125 人；「環境組」的預計成功樣本數為150 人，實際成功樣本數則為131 人。
本計畫面訪正式調查預計完成樣本數為4200 案（綜合組2000 案，環境組2200 案），
為了避免各地區因故拒訪、籍在人不在等因素干擾而致無法達成預計目標，乃採預先膨脹
樣本數的方式因應，來估算每一村里所需抽取之人數，膨脹係數之估算乃依據兩年內調研
中心所執行類似大型計畫的完訪率來決定，各鄉鎮膨脹係數介於1.3 倍至2.5 倍之間，綜合
組共抽出4018 案，而環境組共抽出4602。
抽樣工作分預試與正式調查兩次進行。抽樣程序如下：
A．鄉鎮市（區）的抽樣：
1. 根據行政院內政部民國99 年2 月底人口統計檔查閱所有鄉鎮（區）別及各鄉鎮市（區）
的人口數。
2. 再依據「人口密度」、「教育程度」、「65 歲以上人口百分比」、「15-64 歲人口百分比」、
「工業人口百分比」、「商業人口百分比」等指標，將台灣之鄉鎮市區分為七層。將同
分層之鄉鎮市區按人口數由小到大排列，並做人口數的累加。
3. 以該層的總人口數T 除以所需的鄉鎮市區數ｎ，Ｔ／ｎ＝Ｋ（Ｋ四捨五入，留下整數
部分）。
4. 在１～Ｋ中取一個亂數R，則R 在累加人口數該欄的落點所在即是抽取的第一個鄉鎮
市（區）。
5. 然後以R 為基礎，再往下找第R＋K 的落點，以此類推，直到抽滿所需的鄉鎮市區數
為止。
B．村里的抽樣
1. 根據行政院內政部民國99 年2 月底人口統計檔。
2. 依照其排列各村里的原次序做人口數的累加。
3. 以該鄉鎮市區的總人口數Ｔ除以所需的村里數ｎ，Ｔ／ｎ＝Ｋ（Ｋ四捨五入，留下整
數部分）。
4. 在１～Ｋ中取一個亂數Ｒ，則Ｒ的落點所在即是抽出的第一個村里。
5. 然後以Ｒ為基礎，再往下找到第Ｒ+Ｋ的落點，以此類推，直到抽滿所需的村里數。
C．選取合格者
1. 樣本合格者條件：18 歲以上之中華民國國民。
2. 程序：
（1）查該村里十八歲以上人數Ｈ。
（2）以該村里十八歲以上人數除以所需樣本數ｎ
Ｈ／ｎ＝Ｂ
Ｂ可能是一個帶有小數點的數字如42.31 或53.82…
Ｂ去掉小數點後的數字，留下整數部分（42 或53）Ｂ＊。
（3）從１～Ｂ＊中抽出一個亂數Ｒ，若第Ｒ人為合格者，則為抽中之第一人。
注意：若Ｒ為不合格者，請找Ｒ人的前一人（即Ｒ－１人）為合格者。若仍為不合格者，
則找後一人（即為Ｒ＋１），以此法類推，Ｒ－２，Ｒ＋２……到Ｒ＋３人仍找不
到則放棄，然後以Ｒ為基礎再往下找到Ｒ＋Ｂ＊人。若找到合格者之後，亦以Ｒ
＋Ｂ＊人為基礎往後找。
====================2010 overall====================
資料使用說明
2.	資料檔說明
(1)	複雜抽樣變項：根據計畫小組的抽樣架構，提供抽樣依據的地區分層(Stratum)、第一抽出單位(Primary Sampling Unit, PSU)、第二抽出單位(Secondary Sampling Unit, SSU)等複雜抽樣變項。
(2)	根據計畫小組的抽樣架構(PSU、SSU以及各村里抽取數)，以等比例放大抽取率方式新增不等機率抽樣權數(wsel)；根據計畫小組的抽樣架構與其抽樣母體資料，資料庫以Raking方式新增權值變項(wr)。
(3)	郵遞區號：依據訪問村里所在位置，提供郵遞區號資訊。

權值說明
由於調查之抽樣採取分層三階段pps抽樣法，及膨脹樣本無替換的設計，使得樣本的中選機率是不相等的，這部分將以調查設計的加權權數來處理。表2與表3分別為加權前與加權後之結果。
(一)	不等機率抽樣權值（unequal probabilities of selection, sampling weights）
由於本計畫採用分層三階段PPS抽樣，再加上使用膨脹樣本的策略，使每個人具有不同的中選機率。為了補償此不等機率抽樣的問題，在資料處理上將採不等機率加權的措施。首先，分層三階段PPS抽樣的抽取率計算方式如下：
將所有成功樣本之不等機率抽樣權值( )加總，亦可得到全國成功母體總數( )，也就是一個成功樣本，在台灣地區代表多少人。為使每個成功樣本 權值加總等於成功樣本數(n)，需重新調整 為 ，其計算方式為：
(二)	多變項反覆加權法的權值
為使成功樣本結構具有代表性並符合母體結構，在完成前述不等機率加權權值 後，隨即針對「性別」、「年齡」（六分類）、「教育程度」（五分類）與「地區層別」（六分類）四個變項進行樣本代表性檢定（卡方檢定），並採用「多變項反覆加權法」進行加權，直到成功樣本代表性檢定結果符合母體的分佈狀況為止。由表十三和表十四之結果顯示成功之樣本與母體資料均無顯著差異，則表示反覆加權過後成功樣本具有代表性。各分類加權權值的計算公式如下：

抽樣說明
同2010env
====================2016 citizen====================
資料使用說明
(1)	複雜抽樣變項：根據計畫小組的抽樣架構，提供抽樣依據的地理分層(r_stratum2014)、第一抽出單位(Primary Sampling Unit, PSU)、第二抽出單位(Secondary Sampling Unit, SSU)等複雜抽樣變項。
(2)	郵遞區號：依據訪問村里所在位置，提供郵遞區號資訊。
(3)	提供4個加權權值變項，請參考調查報告附錄十三資料檔加權說明。

權值說明
本次加權工作所使用之母體資料是由內政部所提供之104年度12月份人口統計資料為準，扣除外島人口，包括連江縣、金門縣等地區後，總人口數為19,322,488人，兩組抽出樣本數，家庭組為4,076案，公民與國家組為4,070案（見「貳、抽樣設計」），但實際調查訪問後，家庭組成功樣本數為2,024案、公民與國家組成功樣本數為1,966案。
在資料收集完後，我們必須先檢查成功樣本的「性別」、「年齡」、「教育程度」及「地理分層」是否與母體人口結構一致。就教育程度14方面，我們分別考慮「教育程度四分類」和「教育程度五分類」，其中兩者主要差異在於「教育程度四分類」是將「不識字/自修/小學」視為一類，而「教育程度五分類」則是分為「不識字」與「自修/小學」兩類。而兩者其餘的三分類皆為「國中（初）中/初職」、「高中普通科/高中職業科/高職/士官學校」及「五專/二專/三專/軍警校專修班/軍警校專科班/空中行專/空中商專/空中大學/技術學院、科技大學/大學/碩士/博士」。由表一為公民組其結果顯示成功樣本於「教育程度」和「年齡」偏離母體人口結構，而由表三為宗教組其結果顯示成功樣本於「教育程度」偏離母體人口結構，且「教育程度」不論為何種分類，兩組之檢定結果皆為顯著。因此，調研中心在資料釋出時，將分別提供每個成功樣本「不等機率加權權值」以及「多變項反覆加權法的權值」。
(一) 不等機率抽樣權值（unequal probabilities of selection, sampling weights）
由於本計畫採用分層多階段PPS抽樣，再加上使用膨脹樣本的策略，使每個人具有不同的中選機率。為了補償此不等機率抽樣的問題，在資料處理上將採不等機率加權的措施。分層多階段PPS抽樣的抽取率計算方式如下：
1、各分層裡每個人的原中選機率
2、膨脹樣本後的個人中選機率
加權權值的計算公式：wsel0=1f（county）
將所有成功樣本之不等機率抽樣權值（wsel0）加總，亦可得到全國成功母體總數（Nv），也就是一個成功樣本，在台灣地區代表多少人。為使每個成功樣本wsel0權值加總等於成功樣本數（n），需重新調整wsel0為wsel，其計算方式為：
(二) 多變項反覆加權法的權值
為使成功樣本結構具有代表性並符合母體人口結構，在完成前述不等機率加權權值wsel後，隨即針對「性別」、「年齡」、「教育程度」與「地理分層」四個變項進行樣本代表性檢定（卡方檢定），並採用「多變項反覆加權法」進行加權，直到成功樣本代表性檢定結果符合母體人口結構的分佈狀況為止。而各分類加權權值的計算公式如下：
其中將以wr4和wr5分別表示利用「教育程度四分類」和「教育程度五分類」所得到的權值。而以wr4為權值之表二（公民組）與表四（宗教組），以及wr5為權值之表二（公民組）與表四（宗教組）之結果皆顯示成功之樣本與母體特徵均無顯著差異，表示反覆加權過後成功樣本具有代表性。

抽樣說明
（一）抽樣母體
以台灣地區（不含福建省金門縣和連江縣）具有戶籍，年齡在18歲（含）以上民眾（民國86年12月31日以前出生）的戶籍資料檔為抽樣母體，故以戶籍資料作為抽樣清冊（sampling frame）。實際調查訪問時，並不包括軍事單位、醫院、療養院、學校、職訓中心、宿舍、監獄等機構內之居民及通緝犯；而調查訪問地區則是以台灣地區為主要的訪查地點。
（二）抽樣分層
本調查採用的中央研究院人文社會科學研究中心調查研究專題中心與社會變遷基本調查計畫於民國103年共同研發的分層，2是依據人口結構變項與人文區位的經濟變項，包含「農林漁牧從業人口百分比」、「工業從業人口百分比」、「職業等級_專業及主管人員人口百分比」、「十五至六十四歲人口百分比」、「六十五歲及以上人口百分比」、「大學及以上教育人口百分比」、「人口密度」與「5年人口成長數」八個變項，將台灣地區358個鄉鎮市區分為七個層別。在考量地理區因素後，將七個層別調整合併為19個分層，並稱之為地理分層。
（三）抽樣方法
樣本的選取採用「分層多階段PPS抽樣法（stratified multi-stage probability proportional tosize （PPS） sampling）」，各層內採用抽取率與單位大小成比例（PPS）等距抽樣法，逐步抽取「地理分層」、「鄉鎮市區」、「村里」、「人」。本計畫預試調查的抽樣方式採四階段抽樣，而正式調查的抽樣方式主要是採三階段抽樣，惟為了強化花蓮與台東地理分層的區域代表性，僅就有此地理分層採二階段抽樣。
(1) 二階段抽樣：第一階段的單位為「村里」，接著抽出「人」。該地理分層之「鄉鎮市區」全部涵蓋在內。
(2) 三階段抽樣：第一階段的單位為「鄉鎮市區」，其次抽出「村里」，最後抽出「人」。
(3) 四階段抽樣：第一階段的單位為「地理分層」，第二階段的單位為「鄉鎮市區」，第三階段的單位為「村里」，最後抽出「人」。
（四）膨脹樣本
為了避免因拒訪、不合格樣本（如服役、死亡等）及無法接觸的樣本（如不住原址）等因素的干擾，而致無法達成預計目標。因此，在實際執行抽樣時，參照近三年內本專題中心所執行的大型計畫完成率，3並依受訪地區狀況調整，決定每一個中選鄉鎮市區的樣本膨脹係數，估算每一鄉鎮市區須抽取的樣本數。本次調查之膨脹係數依上述原則調整後，預試調查介於1.4倍至2.6倍之間，正式調查介於1.4倍至3.0倍之間。
（五）抽樣結果
本計畫兩問卷組（綜合組及工作與生活組）各進行預試及正式調查，茲將其抽樣結果分述如下：
(1) 預試調查
預試調查兩問卷組預計各完成150案，採分層多階段PPS抽樣，使母體中的每個人都有一個不為零（non-zero）的中選機會，每一個中選的個案，均會請訪員努力去接觸及完成訪問。
為了節省調查成本，將排除花蓮與台東地區，並以五個「地理區」依序北北基宜、桃竹苗、中彰投、雲嘉南與高屏澎，僅各抽取一個地理分層進行調查。先依據內政部戶政司所提供民國104年12月底的人口統計資料計算各地理區的人口比例，再依所計算出的人口比例計算各地理區所需抽取的鄉鎮市區數，並調整抽取村里數與各村里應完成數，使得兩組問卷應完成樣本數均為150案。抽樣設計請參見表2-1，中選地理分層及鄉鎮市區請參見表2-2。在實際執行抽樣時，採用分層四階PPS抽樣法，其中第一階段抽取地理分層、第二階段抽取鄉鎮、第三階段抽取村里。最後以台灣地區戶籍資料作為抽樣清冊，由本專題中心自內政部戶政司申請調查單位之戶籍資料檔內抽出個人樣本。
由於本調查運用膨脹樣本且樣本無替代的設計，故調整後兩組預定抽出樣本數分別為綜合組300案和工作與生活組311案。各鄉鎮調整後抽出樣本數及膨脹係數如表2-3。
(2) 正式調查
正式調查兩問卷組預計各完成2,000案，採分層多階段PPS抽樣，使母體中的每個人都有一個不為零的中選機會，每一個中選的個案，均會請訪員努力去接觸及完成訪問。
依據內政部戶政司所提供民國104年12月底的人口統計資料計算每一分層的人口比例，並依所計算出的人口比例計算各分層所需抽取的鄉鎮市區數，再經由抽取村里數與各村里應完成數調整過後，兩組問卷應完成樣本數為2,004案，抽樣設計請參見表2-4，中選地理分層及鄉鎮市區如表2-5。鑒於花東地區人口數過少及人口密度分佈極不平均，為了確保取樣能充分代表花東地區，在實際執行抽樣時，採用分層二階段PPS抽樣法，其他地區仍採用分層三階段PPS抽樣法抽取個人。最後以台灣地區戶籍資料作為抽樣清冊，由本專題中心自內政部戶政司申請調查單位之戶籍資料檔內抽出樣本。
由於本調查運用膨脹樣本且樣本無替換的設計，故調整後兩組預定抽出樣本數分別為家庭組4,076案和公民與國家組4,070案。各鄉鎮調整後抽出樣本數及膨脹係數如表2-6。

  "
}




# 第七部份：把問卷資料變形以便串連及行政區、選舉資料 ---------------------------------
load(file=paste0(dataset_in_scriptsfile_directory,"miced_survey_9_with_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(save_dataset_in_scriptsfile_directory,"miced_survey_2surveysonly_mirt_lca_clustering.RData"), verbose=TRUE)
load(file=paste0(dataset_in_scriptsfile_directory,"survey_data_with_condensed_opinion.RData"), verbose=TRUE)

recode_condense_label_basis<-dplyr::filter(survey_codebook,grepl(pattern="construct", ID)) %>% dplyr::mutate(newlabel=paste0("[",VALUE,"] ",LABEL))
recode_condense_label_basislist<-magrittr::set_names(recode_condense_label_basis$newlabel,recode_condense_label_basis$LABEL)

survey_data_imputed_with_newconstruct<-lapply(names(survey_data_imputed), function(surveykey, ...) {
  retdf<-dplyr::left_join(survey_data_imputed[[surveykey]], survey_data_with_condensed_opinion[[surveykey]])
  constructcols<-grep(pattern="construct", x=names(retdf), value=TRUE)
  dplyr::mutate_at(retdf, constructcols, dplyr::recode_factor, !!!recode_condense_label_basislist)
}, survey_data_imputed=survey_data_imputed, survey_data_with_condensed_opinion=survey_data_with_condensed_opinion, recode_condense_label_basislist=recode_condense_label_basislist)
survey_data_imputed<-magrittr::set_names(survey_data_imputed_with_newconstruct, names(survey_data_imputed))
#library(reshape2)
#survey_oldq_id<-list(
#  "2004citizen"=c("v25","v26","v27","v41","v42","v43","v44","v45","v46","v60","v61","v62","v65","v74","v91a","v91b","v92_1","v92_2","v92_3","v92_4","v92_5","v93a","v93b","v95","v96","v97","v105a","v105b","v105c","v106a","v106b","v106c","v107a","v107b","v107c","v114","v118a","v118b","v118c","v118d"),
#  "2010env"=c("v39a", "v39b", "v39c", "v40", "v78a", "v78b", "v78c", "v78d", "v78e", "v78f", "v78g", "v78h", "v78i", "v90", "v91", "v92"),
#  "2010overall"=c("kv21c_0", "kv31_0", "kv67_0", "v14a", "v14b", "v15a", "v15b", "v16a", "v16b", "v19", "v20a", "v20b", "v21c", "v22a", "v22b", "v22c", "v23a", "v23b", "v23c", "v24a", "v24b", "v24c", "v25a", "v25b", "v25c", "v26a", "v26b", "v26c", "v26d", "v26e", "v26f", "v26g", "v27a", "v27b", "v27c", "v27d", "v27e", "v27f", "v27g", "v28a", "v28b", "v29", "v30a", "v30b", "v31", "v32a", "v32b", "v32c", "v36a", "v36b", "v37a", "v37b", "v37c", "v37d", "v37e", "v37f", "v37g", "v37h", "v37i", "v38a1", "v38a2", "v38b1", "v38b2", "v38c1", "v38c2", "v38d1", "v38d2", "v38e1", "v38e2", "v39a", "v39b", "v39c", "v40", "v57", "v58", "v59", "v63", "v66c", "v66f", "v67", "v68", "v69", "v70b", "v70c", "v70d", "v70e", "v70f"),
#  "2016citizen"=c("c1a",	"c1b",	"c1c",	"c1d",	"c1e",	"c2",	"c3",	"c4",	"c5",	"c6",	"c10",	"c11",	"c12",	"c13",	"c14",	"d1",	"d2a",	"d2b",	"d3a",	"d3b",	"d4",	"d5a",	"d5b",	"d5c",	"d5d",	"d5e",	"d5f",	"d6a",	"d6b",	"d6c",	"d6d",	"d6e",	"d6f",	"d6g",	"d6h",	"d7a",	"d7b",	"d7c",	"d7d",	"d7e",	"d7f",	"d7g",	"d7h",	"d7i",	"d7j",	"d7k",	"d8a",	"d8b",	"d8c",	"d11a",	"d11b",	"d12",	"d13a",	"d13b",	"d14a",	"d14b",	"d14c",	"d17a",	"d17b",	"d17c",	"e2a",	"e2b",	"e2c",	"e2d",	"e2e",	"e2f",	"e2g",	"e2h",	"e2i",	"f3",	"f4",	"f5",	"f8",	"f9",	"h10")
#)
extracting_topicitems_from_survey<-function(X,df,oldvec=c(), topickeyword=c("議題","議題（或民主價值與公民意識牽涉群體）","民主價值與公民意識")) {
  if(identical(oldvec,c())) {
    oldvec[[X]]=c()
  }
  needq<-dplyr::filter(df,SURVEY==X,CATEGORY %in% topickeyword) %>%
    dplyr::select(ID) %>%
    unlist() %>%
    as.character() %>%
    union(oldvec[[X]])
  return(needq)
}

survey_q_ids<-sapply(survey_data_title,extracting_topicitems_from_survey,df=survey_imputation_and_measurement)
survey_q_on_pp<-sapply(survey_data_title,extracting_topicitems_from_survey,df=survey_imputation_and_measurement, topickeyword=c("參與"))

#有些資料在轉換過程中內容會變成label而非coding的資料，要把他變回來


if({covert_label_according_to_xls_codebook<-FALSE;covert_label_according_to_xls_codebook}) {
  mistakinglevelvars<-list(
    "2004citizen"=c('myown_indp_atti','v61','v62','v74','v91a','v91b','v93a','v93b','v95'),
    "2010env"=c('v14a','v14b','v16a','v16b','v20a','v20b','v28a','v28b','v31','v40','v57','v58','v59'),
    "2010overall"=c('myown_indp_atti','v61','v62','v80','v89'),
    "2016citizen"=c('myown_indp_atti','c1a','c1b','c1c','c1d','c1e','c2','c3','c6','c8','c8r','c9','c9r','c11','c14','d1','d4','d8a','d8b','d8c','d9a','d9b','d10','h10','h10r')
  )
  #以下部分是從先前問卷舊標籤factor方式而來，如果要使用還需要重新修改
  prepare_for_label_adj_df<-prepare_for_label_adj_df
  mistakinglevelvars<-mistakinglevelvars
  dfcodebook<-survey_codebook
  prepare_for_label_adj_df <- lapply(mistakinglevelvars,function(mistakinglevelvar,...) {
    dfcodebook<-dfcodebook[
      (dfcodebook$SURVEY==survey_data_title) & (dfcodebook$ID == mistakinglevelvar),
      c('SURVEY','ID','VALUE','LABEL')]#ID %in% Y
    dedf_keyvalues<-as.list(getElement(dfcodebook,'VALUE'))
    names(dedf_keyvalues)<-getElement(dfcodebook,'LABEL')
    #result<-filter(dfcodebook,SURVEY==X,ID %in% Y)
    dedf_keyvalues
  },MoreArgs=list(survey_data_title=survey_data_title,dfcodebook=dfcodebook),
  SIMPLIFY=FALSE)
  names(prepare_for_label_adj_df)<-mistakinglevelvars
  for (recodevar in mistakinglevelvars) {
    tplistforrecode <- getElement(prepare_for_label_adj_df,recodevar)
    X[[recodevar]] <- dplyr::recode(getElement(X,recodevar),!!!tplistforrecode)
    #message("recodevar is ", recodevar," and length is ",length(X$recodevar)," and list is ",tplistforrecode," and names of list is",names(tplistforrecode))
  }
  #X
}

needimps<-custom_ret_appro_kamila_clustering_parameters() %>%
  dplyr::rename(SURVEY=survey) %>%
  dplyr::select(-.imp)
needsurveys<-names(survey_data_imputed)

# test survey reliability --------------
reliability_test_res<- custom_parallel_lapply(1:nrow(needimps), function(rowi, ...) {
  needrow<-needimps[rowi,]
  adj_survey_q_ids<-grep(pattern="construct", x=survey_q_ids[[needrow$SURVEY]], value=TRUE) %>%
    base::setdiff(survey_q_ids[[needrow$SURVEY]], .)
  basesurveydf<-dplyr::filter(survey_data_imputed[[needrow$SURVEY]], .imp==!!needrow$imp)
  testres_q<-adj_survey_q_ids %>%
    basesurveydf[,.] %>%
    dplyr::mutate_all(unclass) %>%
    psych::alpha(check.keys=TRUE)
  #readline(paste("now in",needrow$SURVEY,needrow$imp,"testres_q, continue?"))
  testres_pp<-survey_q_on_pp[[needrow$SURVEY]] %>%
    basesurveydf[,.] %>%
    dplyr::mutate_all(unclass) %>%
    psych::alpha(check.keys=TRUE)
  #print(testres_pp)
  data.frame(survey=needrow$SURVEY, imp=needrow$imp, items=c("policy","pp"), alphares=c(testres_q$total$std.alpha, testres_pp$total$std.alpha))
  #readline(paste("now in",needrow$SURVEY,needrow$imp,"testres_pp, continue?"))
}, survey_data_imputed=survey_data_imputed, needimps=needimps, survey_q_on_pp=survey_q_on_pp, survey_q_ids=survey_q_ids, method=parallel_method) %>%
  plyr::rbind.fill()
write.csv(reliability_test_res, file=paste0("TMP.csv"))

# test relation reliability with Krippendorff's alpha --------------

newt<-openxlsx::read.xlsx(paste0(dataset_file_directory,"interrater.xlsx"), sheet=2)
oldt<-openxlsx::read.xlsx(paste0(dataset_file_directory,"interrater.xlsx"), sheet=4)
all_coding_option<-dplyr::bind_rows(newt,oldt)[,1] %>% unique()
relation_reliability<-data.frame(both=all_coding_option) %>%
  dplyr::mutate(in_newt=both %in% !!newt$relation) %>%
  dplyr::mutate(in_oldt=both %in% !!oldt$relation)
as.matrix(relation_reliability[,c("in_newt","in_oldt")]) %>%
  irr::kripp.alpha(., method="nominal")

# compacting (wide to long) surveys --------------

#survey_data_melted
complete_survey_dataset <- mapply(function(X,Y) {
  survey_data_title<-X$SURVEY[1]
  X<-dplyr::mutate_at(X,Y,as.character) %>%
    dplyr::mutate(myown_age_grpscaled=as.numeric(scale(myown_age)))
  if (survey_data_title=="2010overall") {
    X %<>% dplyr::mutate(stratum=as.character(paste0("2010overall",stratum2)))
  }
  if (survey_data_title=="2016citizen") {
    X %<>% dplyr::mutate(stratum=as.character(paste0("2016citizen",r_stratum2014))) %>%
      mutate_cond(is.na(ssu), ssu="花東不抽樣")
  }
  if (survey_data_title %in% c("2010overall","2016citizen")) {
    X %<>% dplyr::mutate_at(c("psu","ssu"),~as.character(paste0(!!survey_data_title,as.character(.))))
  }
  
  if (survey_data_title %in% c("2010overall")) {
    X %<>% dplyr::mutate_at(c("admindistrict","adminvillage"), as.character)
    #內湖區 | 内湖區
    #内湖區    端陽里
    #板橋市 廣徳里 | 廣德里(legislator_with_elec)
    #蘆竹鄉    内厝村 | 內厝村(legislator_with_elec)
    #蘆竹鄉    瓦薰村 | 瓦窯村(legislator_with_elec)
    #烏日鄉    仁徳村 | 仁德村(legislator_with_elec)
    #西區    磚瑤里 | 磚磘里(legislator_with_elec)
    #嘉義市 西區    西榮里 | 
    #内埔鄉    東寧村 | 內埔鄉(legislator_with_elec)
    #内埔鄉    内田村 | 內埔鄉(legislator_with_elec)
    #崁頂鄉    圍内村 | 圍內村(legislator_with_elec)
    #崁頂鄉    炭頂村 | 崁頂村(legislator_with_elec)
    X %<>% mutate_cond(admindistrict=="内湖區", admindistrict="內湖區") %>%
      mutate_cond(adminvillage=="端陽里", adminvillage="瑞陽里") %>%
      mutate_cond(adminvillage=="廣徳里", adminvillage="廣德里") %>%
      mutate_cond(adminvillage=="内厝村", adminvillage="內厝村") %>%
      mutate_cond(adminvillage=="瓦薰村", adminvillage="瓦窯村") %>%
      mutate_cond(adminvillage=="仁徳村", adminvillage="仁德村") %>%
      mutate_cond(adminvillage=="磚瑤里", adminvillage="磚磘里") %>%
      mutate_cond(admincity=="嘉義市" & admindistrict=="西區" & adminvillage=="西榮里", adminvillage="西平里") %>%
      mutate_cond(adminvillage=="廣徳里", adminvillage="廣德里") %>%
      mutate_cond(admindistrict=="内埔鄉", admindistrict="內埔鄉") %>%
      mutate_cond(adminvillage=="内田村", adminvillage="內田村") %>%
      mutate_cond(adminvillage=="圍内村", adminvillage="圍內村") %>%
      mutate_cond(adminvillage=="炭頂村", adminvillage="崁頂村")
    X %<>% dplyr::mutate_at(c("admindistrict","adminvillage"), as.factor)
  }
  
  if (survey_data_title %in% c("2016citizen")) {
    X %<>% dplyr::mutate_at(c("admindistrict","adminvillage"), as.character)
    X %<>% mutate_cond(admincity=="屏東縣" & admindistrict=="屏東市" & adminvillage=="民權里", adminvillage="光榮里、民權里")
    #屏東市    民權里 | 光榮里、民權里(legislator_with_elec)
    X %<>% dplyr::mutate_at(c("admindistrict","adminvillage"), as.factor)
  }
  X<-dplyr::mutate(X,myown_income_scaled=as.numeric(scale(myown_income)))
  
  other_var_as_id<-base::setdiff(names(X),Y)
  reshape2::melt(X, id.vars = other_var_as_id, variable.name = "SURVEYQUESTIONID", value.name = "SURVEYANSWERVALUE") %>%
    dplyr::mutate_at("SURVEYANSWERVALUE", as.character)
},X=survey_data_imputed[needsurveys],Y=survey_q_ids[needsurveys]) %>%
  {#節省欄位合併
    common_var<-Reduce(base::intersect, lapply(., names )) %>%
      base::setdiff(c("sd")) %>% c("psu","ssu","stratum")
    lapply(., select_and_fill_nonexistcol, common_var)
  } %>%
  plyr::rbind.fill() %>%
  #dplyr::bind_rows() %>%
  dplyr::rename(ansv_and_label=SURVEYANSWERVALUE, imp=.imp, id_of_imp=.id) %>%
  dplyr::mutate("value_on_q_variable"=paste0(SURVEY,"@",SURVEYQUESTIONID)) %>%
  dplyr::select(-tidyselect::any_of(c("zip","village","wave","qtype","myown_industry","myown_job","villagefullname","myown_family_income_ingroup","SURVEYQUESTIONID"))) %>%
  dplyr::select(-tidyselect::any_of(c("term1","term2","year","year_m","sm"))) %>%#,-sd,-myown_int_pol_efficacy,-myown_ext_pol_efficacy,-myown_constituency_party_vote
  dplyr::select(!dplyr::ends_with("NA")) %>%
  dplyr::select(-tidyselect::any_of(c("myown_eduyr","myown_occp","myown_ses","myown_income","myown_dad_ethgroup","myown_mom_ethgroup","myown_working_status","myown_job_status","myown_familymembers_num","myown_selfid_population","myown_hire_people_no","myown_manage_people_no","myown_constituency_party_vote","myown_online_time"))) %>%
  dplyr::mutate_at(c("SURVEY","admincity","admindistrict","adminvillage","value_on_q_variable"),as.factor) %>%
  dplyr::mutate_at("id", as.integer) %>%
  dplyr::mutate_at(c("stratum","psu","ssu"), as.factor) %>%
  dplyr::filter(SURVEY %in% c("2010overall","2016citizen")) %>%
  dplyr::semi_join(needimps) %>%
  dplyr::left_join(needimps)

# t <- complete_survey_dataset %>%
#   #dplyr::semi_join(needimps) %>%
#   #dplyr::left_join(needimps) %>%
#   dplyr::filter(newimp==1) %>%
#   dplyr::mutate(survey_with_id=paste0(SURVEY,id))
# unique(t$survey_with_id) %>% length()

#complete_survey_dataset %<>% data.table::as.data.table()
#save(complete_survey_dataset,file=paste0(dataset_in_scriptsfile_directory, "complete_survey_dataset.RData"))

#View(filter(complete_survey_dataset[[1]],SURVEYQUESTIONID=='myown_indp_atti'))
#dplyr::recode(survey_data_test[[1]]$v61,!!!getElement(getElement(prepare_for_label_adj_df,"2004citizen"),"v61"))
#vhead(mergedf_votes_bills_surveyanswer)
#vhead(complete_survey_dataset)
#以下是要把四份問卷合一，但這應該要放棄



#survey_data_melted 沒有節省欄位直接合併
#complete_survey_dataset<-Reduce(plyr::rbind.fill,complete_survey_dataset) %>%
#  extract(common_var)
#vhead(complete_survey_dataset)
#survey_data_melted_names<-lapply(survey_data_melted,names)


#factor to numeric method
#survey_data_melted<-lapply(survey_data_melted,function(X) {
#  X<-mutate_at(c(),as.numeric(levels(f))[f]

#%>%
#reshape2::melt(id.vars = setdiff(colnames(.),c("term1","term2")), variable.name = "variable_on_term", value.name = "term")
#vhead(complete_survey_dataset %>% filter(SURVEYQUESTIONID=='myown_indp_atti'))
#withoutlabelansv <- unique(complete_survey_dataset$ansv_and_label)[c(31,104:116,135:144,167:173,180:188,209:222,285:287,293:299)]
#filter(complete_survey_dataset, ansv_and_label %in% withoutlabelansv) %>%
#  distinct(SURVEY,SURVEYQUESTIONID,ansv_and_label) %>%
#  View()
#c(NA,"","以上皆非等待發明  不知道何種替代能源","用垃圾科技轉換能源",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

##針對調查問卷資料處理變形，以便合併

#"c1a","c1b","c1c","c1d","c1e","c2","c3","c4","c5","c6","c10","c11","c12","c13","c14","d1","d2a","d2b","d3a","d3b","d4","d5a","d5b","d5c","d5d","d5e","d5f","d6a","d6b","d6c","d6d","d6e","d6f","d6g","d6h","d7a","d7b","d7c","d7d","d7e","d7f","d7g","d7h","d7i","d7j","d7k","d8a","d8b","d8c","d11a","d11b","d12","d13a","d13b","d14a","d14b","d14c","d17a","d17b","d17c","e2a","e2b","e2c","e2d","e2e","e2f","e2g","e2h","e2i","f3","f4","f5","f8","f9","h10","kh10"
