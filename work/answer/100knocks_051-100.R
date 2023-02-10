#* ===============================================*
# データサイエンス 100本ノック（P-051～P-100）
#* ===============================================*
library(tidyverse)
library(lubridate)

file_path <- "G:/マイドライブ/data4Cs/研修/202111_DS-100knocks/output/"

## P-051 ##
# レシート明細データフレーム（df_receipt）の売上エポック秒（sales_epoch）を日付型（timestamp型）に変換し、
# "日"だけ取り出してレシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）とともに抽出せよ。
# なお、"日"は0埋め2桁で取り出すこと。データは10件を抽出すれば良い。
df_receipt[1:10, ] %>%
  mutate(sales_day = format(as.POSIXct(sales_epoch, origin = "1970-01-01"), "%d")) %>%
  select(receipt_no, receipt_sub_no, sales_day, sales_ymd)


## P-052 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計の上、
# 売上金額合計に対して2000円以下を0、2000円超を1に2値化し、顧客ID、売上金額合計とともに10件表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount = sum(amount)) %>%
  mutate(flg = if_else(amount <= 2000, 0, 1))


## P-053 ##
# 顧客データフレーム（df_customer）の郵便番号（postal_cd）に対し、東京（先頭3桁が100〜209のもの）を1、
# それ以外のものを0に２値化せよ。さらにレシート明細データフレーム（df_receipt）と結合し、全期間において
# 買い物実績のある顧客数を、作成した2値ごとにカウントせよ。
df_customer %>%
  mutate(flg = if_else(between(as.numeric(substr(postal_cd, 1, 3)), 100, 209), 1, 0)) %>%
  #  select(postal_cd, flg, address) %>% head(100)
  inner_join(df_receipt, by = "customer_id") %>%
  group_by(flg) %>%
  summarise(count = length(customer_id))


## P-054 ##
# 顧客データデータフレーム（df_customer）の住所（address）は、埼玉県、千葉県、東京都、神奈川県の
# いずれかとなっている。都道府県毎にコード値を作成し、顧客ID、住所とともに抽出せよ。
# 値は埼玉県を11、千葉県を12、東京都を13、神奈川県を14とすること。結果は10件表示させれば良い。
df_customer %>%
  rowwise() %>%
  mutate(pref_cd = switch(substr(address, 1, 3),
    "埼玉県" = "11",
    "千葉県" = "12",
    "東京都" = "13",
    "神奈川" = "14"
  )) %>%
  ungroup() %>%
  select(customer_id, pref_cd, address) %>%
  head(10)


## P-055 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計し、
# その合計金額の四分位点を求めよ。その上で、顧客ごとの売上金額合計に対して以下の基準でカテゴリ値を作成し、
# 顧客ID、売上金額と合計ともに表示せよ。カテゴリ値は上から順に1〜4とする。結果は10件表示させれば良い。
#   ・最小値以上第一四分位未満
#   ・第一四分位以上第二四分位未満
#   ・第二四分位以上第三四分位未満
#   ・第三四分位以上
df_receipt %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  summarise(
    amount_q1 = quantile(amount_sum, 0.25),
    amount_q3 = quantile(amount_sum, 0.75)
  )
df_receipt %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(rank = ntile(amount_sum, 4)) %>%
  head(10)


## P-056 ##
# 顧客データフレーム（df_customer）の年齢（age）をもとに10歳刻みで年代を算出し、顧客ID（customer_id）、
# 生年月日（birth_day）とともに抽出せよ。ただし、60歳以上は全て60歳代とすること。
# 年代を表すカテゴリ名は任意とする。先頭10件を表示させればよい。
df_customer %>%
  rowwise() %>%
  mutate(generation = min(trunc(age / 10) * 10, 60)) %>%
  ungroup() %>%
  select(age, generation) %>%
  head(10)


## P-057 ##
# 前問題の抽出結果と性別（gender）を組み合わせ、新たに性別×年代の組み合わせを表すカテゴリデータを作成せよ。
# 組み合わせを表すカテゴリの値は任意とする。先頭10件を表示させればよい。
df_customer %>%
  rowwise() %>%
  mutate(generation = min(trunc(age / 10) * 10, 60)) %>%
  mutate(flg = switch(gender,
    "男性" = paste0("M_", generation),
    "女性" = paste0("F_", generation),
    paste0("X_", generation)
  )) %>%
  ungroup() %>%
  select(age, generation, gender, flg) %>%
  head(10)


## P-058 ##
# 顧客データフレーム（df_customer）の性別コード（gender_cd）をダミー変数化し、顧客ID（customer_id）と
# ともに抽出せよ。結果は10件表示させれば良い。
df_customer %>%
  mutate(
    is_male = if_else(gender_cd == 0, 1, 0),
    is_female = if_else(gender_cd == 1, 1, 0)
  ) %>%
  select(gender_cd, gender, is_male, is_female) %>%
  head(10)


## P-059 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計し、
# 合計した売上金額を平均0、標準偏差1に標準化して顧客ID、売上金額合計とともに表示せよ。
# 標準化に使用する標準偏差は、不偏標準偏差と標本標準偏差のどちらでも良いものとする。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。結果は10件表示させれば良い。
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  summarise(
    ave = mean(amount_sum),
    std = sd(amount_sum)
  )
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(norm = scale(amount_sum)) %>%
  head(10)


## P-060 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計し、
# 合計した売上金額を最小値0、最大値1に正規化して顧客ID、売上金額合計とともに表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。結果は10件表示させれば良い。
df_receipt_smry <- df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount))
tmp <- df_receipt_smry %>%
  summarise(
    min = min(amount_sum),
    max = max(amount_sum)
  ) %>%
  mutate(range = (max - min)) %>%
  print()
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(norm = scale(amount_sum,
    center = tmp["min"],
    scale = tmp["range"]
  )) %>%
  arrange(-amount_sum) %>%
  head(10)
rm(df_receipt_smry, tmp)

# applyを使って簡素化
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(norm = scale(amount_sum,
    center = apply(data.frame(amount_sum), 2, min),
    scale = apply(data.frame(amount_sum), 2, max)
    - apply(data.frame(amount_sum), 2, min)
  )) %>%
  arrange(-amount_sum) %>%
  head(10)


## P-061 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計し、
# 合計した売上金額を常用対数化（底=10）して顧客ID、売上金額合計とともに表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。結果は10件表示させれば良い。
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(log10 = log10(amount_sum)) %>%
  head(10)


## P-062 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに合計し、
# 合計した売上金額を自然対数化(底=e）して顧客ID、売上金額合計とともに表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。結果は10件表示させれば良い。
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(log = log(amount_sum)) %>%
  head(10)


## P-063 ##
# 商品データフレーム（df_product）の単価（unit_price）と原価（unit_cost）から、各商品の利益額を算出せよ。
# 結果は10件表示させれば良い。
df_product %>%
  mutate(profit = unit_price - unit_cost) %>%
  head(10)


## P-064 ##
# 商品データフレーム（df_product）の単価（unit_price）と原価（unit_cost）から、各商品の利益率の全体平均を
# 算出せよ。 ただし、単価と原価にはNULLが存在することに注意せよ。
df_product %>%
  mutate(profit_rate = (unit_price - unit_cost) / unit_price) %>%
  summarise(ave = mean(profit_rate, na.rm = T))


## P-065 ##
# 商品データフレーム（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# ただし、1円未満は切り捨てること。そして結果を10件表示させ、利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）にはNULLが存在することに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(unit_price_new = trunc(unit_cost / (1 - 0.3))) %>%
  mutate(profit_rate = (unit_price_new - unit_cost) / unit_price_new) %>%
  head(10)


## P-066 ##
# 商品データフレーム（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# 今回は、1円未満を四捨五入すること（0.5については偶数方向の丸めで良い）。
# そして結果を10件表示させ、利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）にはNULLが存在することに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(unit_price_new = round(unit_cost / (1 - 0.3))) %>%
  head(10)


## P-067 ##
# 商品データフレーム（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# 今回は、1円未満を切り上げること。そして結果を10件表示させ、利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）にはNULLが存在することに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(unit_price_new = ceiling(unit_cost / (1 - 0.3))) %>%
  head(10)


## P-068 ##
# 商品データフレーム（df_product）の各商品について、消費税率10%の税込み金額を求めよ。
# 1円未満の端数は切り捨てとし、結果は10件表示すれば良い。ただし、単価（unit_price）にはNULLが存在することに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(unit_price_taxed = trunc(unit_price * 1.1)) %>%
  head(10)


## P-069 ##
# レシート明細データフレーム（df_receipt）と商品データフレーム（df_product）を結合し、
# 顧客毎に全商品の売上金額合計と、カテゴリ大区分（category_major_cd）が"07"（瓶詰缶詰）の
# 売上金額合計を計算の上、両者の比率を求めよ。
# 抽出対象はカテゴリ大区分"07"（瓶詰缶詰）の購入実績がある顧客のみとし、結果は10件表示させればよい。
df_receipt %>%
  inner_join(df_product, by = "product_cd") %>%
  mutate(amount07 = if_else(category_major_cd == "07", amount, as.integer(0))) %>%
  group_by(customer_id) %>%
  summarise(
    total = sum(amount),
    cat07 = sum(amount07)
  ) %>%
  mutate(ratio07 = paste0(sprintf("%6.2f", (cat07 / total) * 100), "%")) %>%
  filter(cat07 > 0) %>%
  head(10)


## P-070 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）に対し、顧客データフレーム
# （df_customer）の会員申込日（application_date）からの経過日数を計算し、顧客ID（customer_id）、
# 売上日、会員申込日とともに表示せよ。結果は10件表示させれば良い（なお、sales_ymdは数値、
# application_dateは文字列でデータを保持している点に注意）。
df_receipt %>%
  inner_join(df_customer, by = "customer_id") %>%
  mutate(day_count = as.Date(as.character(sales_ymd), "%Y%m%d") - as.Date(application_date, "%Y%m%d")) %>%
  select(customer_id, sales_ymd, application_date, day_count) %>%
  arrange(customer_id) %>%
  head(10)


## P-071 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）に対し、顧客データフレーム
# （df_customer）の会員申込日（application_date）からの経過月数を計算し、顧客ID（customer_id）、
# 売上日、会員申込日とともに表示せよ。結果は10件表示させれば良い（なお、sales_ymdは数値、
# application_dateは文字列でデータを保持している点に注意）。1ヶ月未満は切り捨てること。
df_receipt %>%
  inner_join(df_customer, by = "customer_id") %>%
  rowwise() %>%
  mutate(month_count = length(seq(
    as.Date(application_date, "%Y%m%d"),
    as.Date(as.character(sales_ymd), "%Y%m%d"),
    "month"
  )) - 1) %>%
  ungroup() %>%
  select(customer_id, sales_ymd, application_date, month_count) %>%
  arrange(customer_id) %>%
  head(10)


## P-072 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）に対し、顧客データフレーム
# （df_customer）の会員申込日（application_date）からの経過年数を計算し、顧客ID（customer_id）、
# 売上日、会員申込日とともに表示せよ。結果は10件表示させれば良い。（なお、sales_ymdは数値、
# application_dateは文字列でデータを保持している点に注意）。1年未満は切り捨てること。
df_receipt %>%
  inner_join(df_customer, by = "customer_id") %>%
  rowwise() %>%
  mutate(year_count = length(seq(
    as.Date(application_date, "%Y%m%d"),
    as.Date(as.character(sales_ymd), "%Y%m%d"),
    "year"
  )) - 1) %>%
  ungroup() %>%
  select(customer_id, sales_ymd, application_date, year_count) %>%
  arrange(customer_id) %>%
  head(10)


## P-073 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）に対し、顧客データフレーム
# （df_customer）の会員申込日（application_date）からのエポック秒による経過時間を計算し、
# 顧客ID（customer_id）、売上日、会員申込日とともに表示せよ。結果は10件表示させれば良い
# （なお、sales_ymdは数値、application_dateは文字列でデータを保持している点に注意）。
# なお、時間情報は保有していないため各日付は0時0分0秒を表すものとする。
df_receipt %>%
  inner_join(df_customer, by = "customer_id") %>%
  mutate(
    day_count = as.Date(as.character(sales_ymd), "%Y%m%d") - as.Date(application_date, "%Y%m%d"),
    epoch_count = as.integer(day_count * (60 * 60 * 24))
  ) %>%
  select(customer_id, sales_ymd, application_date, day_count, epoch_count) %>%
  arrange(customer_id) %>%
  head(10)


## P-074 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）に対し、当該週の月曜日からの経過日数を計算し、
# 売上日、当該週の月曜日付とともに表示せよ。結果は10件表示させれば良い（なお、sales_ymdは数値でデータを
# 保持している点に注意）。
df_receipt %>%
  mutate(
    sl_dt = ymd(sales_ymd),
    wday = wday(sl_dt),
    count = if_else(wday == 1, 6, wday - 2),
    MON = sl_dt - count
  ) %>%
  select(sales_ymd, sl_dt, wday, count, MON) %>%
  head(10)


## P-075 ##
# 顧客データフレーム（df_customer）からランダムに1%のデータを抽出し、先頭から10件データを抽出せよ。
set.seed(100)
df_customer %>%
  sample_n(10)


## P-076 ##
# 顧客データフレーム（df_customer）から性別（gender_cd）の割合に基づきランダムに10%のデータを
# 層化抽出データし、性別ごとに件数を集計せよ。
df_customer %>%
  group_by(gender_cd) %>%
  summarise(count = length(customer_id))
set.seed(100)
df_customer %>%
  group_by(gender_cd) %>%
  sample_frac(0.1) %>%
  summarise(count = length(customer_id))


## P-077 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客単位に合計し、合計した売上金額の
# 外れ値を抽出せよ。ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
# なお、ここでは外れ値を平均から3σ以上離れたものとする。結果は10件表示させれば良い。
df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  filter(amount_sum >= apply(data.frame(amount_sum), 2, sd) * 3)


## P-078 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客単位に合計し、合計した売上金額の
# 外れ値を抽出せよ。ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
# なお、ここでは外れ値を第一四分位と第三四分位の差であるIQRを用いて、「第一四分位数-1.5×IQR」よりも
# 下回るもの、または「第三四分位数+1.5×IQR」を超えるものとする。結果は10件表示させれば良い。
df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  mutate(
    q1 = apply(data.frame(amount_sum), 2, quantile, probs = 0.25),
    q3 = apply(data.frame(amount_sum), 2, quantile, probs = 0.75),
    iqr = q3 - q1
  ) %>%
  filter((amount_sum < (q1 - iqr * 1.5)) | (amount_sum > (q3 + iqr * 1.5))) %>%
  head(10)


## P-079 ##
# 商品データフレーム（df_product）の各項目に対し、欠損数を確認せよ。
apply(is.na(df_product), 2, sum)


## P-080 ##
# 商品データフレーム（df_product）のいずれかの項目に欠損が発生しているレコードを全て削除した
# 新たなdf_product_1を作成せよ。
# なお、削除前後の件数を表示させ、前設問で確認した件数だけ減少していることも確認すること。
df_product_1 <- na.omit(df_product)
nrow(df_product)
nrow(df_product_1)


## P-081 ##
# 単価（unit_price）と原価（unit_cost）の欠損値について、それぞれの平均値で補完した
# 新たなdf_product_2を作成せよ。なお、平均値について1円未満は四捨五入とし、0.5については
# 偶数寄せでかまわない。補完実施後、各項目について欠損が生じていないことも確認すること。
df_product_2 <-
  df_product %>%
  replace_na(list(
    unit_price = round(apply(df_product["unit_price"], 2, mean, na.rm = T)),
    unit_cost = round(apply(df_product["unit_cost"], 2, mean, na.rm = T))
  ))

apply(is.na(df_product_2), 2, sum)
filter(df_product, is.na(unit_price))
summarise(df_product,
  unit_price = mean(unit_price, na.rm = T),
  unit_cost  = mean(unit_cost, na.rm = T)
)
filter(df_product_2, product_cd %in% c("P040802007", "P050103021"))


## P-082 ##
# 単価（unit_price）と原価（unit_cost）の欠損値について、それぞれの中央値で補完した
# 新たなdf_product_3を作成せよ。なお、中央値について1円未満は四捨五入とし、0.5については
# 偶数寄せでかまわない。補完実施後、各項目について欠損が生じていないことも確認すること。
df_product_3 <-
  df_product %>%
  replace_na(list(
    unit_price = round(apply(df_product["unit_price"], 2, median, na.rm = T)),
    unit_cost = round(apply(df_product["unit_cost"], 2, median, na.rm = T))
  ))

apply(is.na(df_product_3), 2, sum)
filter(df_product, is.na(unit_price))
summarise(df_product,
  unit_price = median(unit_price, na.rm = T),
  unit_cost  = median(unit_cost, na.rm = T)
)
filter(df_product_3, product_cd %in% c("P040802007", "P050103021"))


## P-083 ##
# 単価（unit_price）と原価（unit_cost）の欠損値について、各商品の小区分（category_small_cd）ごとに
# 算出した中央値で補完した新たなdf_product_4を作成せよ。なお、中央値について1円未満は四捨五入とし、
# 0.5については偶数寄せでかまわない。補完実施後、各項目について欠損が生じていないことも確認すること。
df_product_4 <-
  df_product %>%
  group_by(category_small_cd) %>%
  mutate(
    unit_price = if_else(is.na(unit_price), round(median(unit_price, na.rm = T)), unit_price),
    unit_cost = if_else(is.na(unit_cost), round(median(unit_cost, na.rm = T)), unit_cost)
  )
## うまくいかなかった
## replace_na(list(unit_price = round(median(unit_price,na.rm=T)),
##                 unit_cost  = round(median(unit_cost ,na.rm=T))))

apply(is.na(df_product_4), 2, sum)
filter(df_product, is.na(unit_price))
round(tapply(df_product$unit_price, df_product$category_small_cd, median, na.rm = T))
filter(df_product_4, product_cd %in% c("P040802007", "P050103021"))


## P-084 ##
# 顧客データフレーム（df_customer）の全顧客に対し、全期間の売上金額に占める2019年売上金額の割合を計算せよ。
# ただし、販売実績のない場合は0として扱うこと。そして計算した割合が0超のものを抽出せよ。
# 結果は10件表示させれば良い。また、作成したデータにNAやNANが存在しないことを確認せよ。
inner_join(df_customer, df_receipt, by = "customer_id") %>%
  group_by(customer_id, sales_year = trunc(sales_ymd / 10000)) %>%
  summarise(amount = sum(amount)) %>%
  pivot_wider(names_from = sales_year, values_from = amount) %>%
  replace_na(list(`2017` = 0, `2018` = 0, `2019` = 0)) %>%
  mutate(
    total = sum(`2017`, `2018`, `2019`),
    share2019 = `2019` / total * 100
  ) %>%
  filter(share2019 > 0) %>%
  head(10)


## P-085 ##
# 顧客データフレーム（df_customer）の全顧客に対し、郵便番号（postal_cd）を用いて経度緯度変換用
# データフレーム（df_geocode）を紐付け、新たなdf_customer_1を作成せよ。ただし、複数紐づく場合は
# 経度（longitude）、緯度（latitude）それぞれ平均を算出すること。
df_customer_1 <-
  left_join(df_customer, df_geocode, by = "postal_cd") %>%
  group_by(customer_id) %>%
  mutate(longitude = mean(longitude), latitude = mean(latitude))


## P-086 ##  ★要改良
# 前設問で作成した緯度経度つき顧客データフレーム（df_customer_1）に対し、申込み店舗コード
# （application_store_cd）をキーに店舗データフレーム（df_store）と結合せよ。そして申込み店舗の
# 緯度（latitude）・経度情報（longitude)と顧客の緯度・経度を用いて距離（km）を求め、
# 顧客ID（customer_id）、顧客住所（address）、店舗住所（address）とともに表示せよ。
# 計算式は簡易式で良いものとするが、その他精度の高い方式を利用したライブラリを利用しても
# かまわない。結果は10件表示すれば良い。
left_join(rename(df_customer_1, latitude_cst = latitude, longitude_cst = longitude, address_cst = `address.x`, store_cd = application_store_cd),
  rename(df_store, latitude_str = latitude, longitude_str = longitude, address_str = address),
  by = "store_cd"
) %>%
  arrange(store_cd, postal_cd) %>%
  # 緯度：φ、 経度：λ、 距離＝6371 × arccos(sinφ1 × sinφ2 ＋ cosφ1 × cosφ2 × cos(λ1 - λ2) )
  # 6371は赤道半径。資料によっては6378とされていることもある
  mutate(distance = 6371 * acos(sin(latitude_cst) * sin(latitude_str) +
    cos(latitude_cst) * cos(latitude_str) * cos(longitude_cst - longitude_str))) %>%
  select(customer_id, address_cst, address_str, postal_cd, store_cd, distance) %>%
  head(10)

# 1レコードずつの結果を返せなかった。
# mutate(geo_cst=c(longitude_cst, latitude_cst),
#        geo_str=c(longitude_str, latitude_str),
#        distance=geospher::distGeo(geo_cst,geo_str)) %>%


## P-087 ##
# 顧客データフレーム（df_customer）では、異なる店舗での申込みなどにより同一顧客が複数登録されている。
# 名前（customer_name）と郵便番号（postal_cd）が同じ顧客は同一顧客とみなし、1顧客1レコードとなるように
# 名寄せした名寄顧客データフレーム（df_customer_u）を作成せよ。
# ただし、同一顧客に対しては売上金額合計が最も高いものを残すものとし、売上金額合計が同一もしくは
# 売上実績の無い顧客については顧客ID（customer_id）の番号が小さいものを残すこととする。
df_customer_u <-
  inner_join(df_customer, df_receipt, by = "customer_id") %>%
  group_by(customer_id, customer_name, postal_cd) %>%
  summarise(amount = sum(amount), .groups = "drop") %>%
  group_by(customer_name, postal_cd) %>%
  mutate(count = length(customer_id)) %>%
  arrange(customer_name, postal_cd, -amount, customer_id) %>%
  distinct(customer_name, postal_cd, .keep_all = T)
df_customer_u %>% filter(count > 1)


## P-088 ##
# 前設問で作成したデータを元に、顧客データフレームに統合名寄IDを付与したデータフレーム
# （df_customer_n）を作成せよ。ただし、統合名寄IDは以下の仕様で付与するものとする。
#  ・重複していない顧客：顧客ID（customer_id）を設定
#  ・重複している顧客：前設問で抽出したレコードの顧客IDを設定
df_customer_n <-
  left_join(df_customer,
    rename(df_customer_u, customer_id_u = customer_id),
    by = c("customer_name", "postal_cd")
  ) %>%
  mutate(customer_id = if_else(is.na(customer_id_u), customer_id, customer_id_u))
df_customer_n %>%
  filter(customer_name == "金城 沙耶") %>%
  select(customer_id, customer_name, gender, birth_day, postal_cd, address, customer_id_u)


## P-閑話 ##
# df_customer_1, df_customer_nは使わないので削除する。
rm(df_customer_1, df_customer_n, df_customer_u)
rm(list = ls()[grepl("^df_product_", ls())])


## P-089 ##
# 売上実績のある顧客に対し、予測モデル構築のため学習用データとテスト用データに分割したい。
# それぞれ8:2の割合でランダムにデータを分割せよ。
set.seed(100)
nrow(inner_join(df_customer, distinct(df_receipt, customer_id), by = "customer_id"))
df_in <-
  inner_join(df_customer, distinct(df_receipt, customer_id), by = "customer_id") %>%
  sample_frac(size = 0.8)
df_out <-
  inner_join(df_customer, distinct(df_receipt, customer_id), by = "customer_id") %>%
  anti_join(df_in, by = "customer_id")
rm(df_in, df_out)


## P-090 ##
# レシート明細データフレーム（df_receipt）は2017年1月1日〜2019年10月31日までのデータを
# 有している。売上金額（amount）を月次で集計し、学習用に12ヶ月、テスト用に6ヶ月の
# モデル構築用データを3セット作成せよ。
df_receipt_sum <-
  df_receipt %>%
  group_by(sales_month = as.character(trunc(sales_ymd / 100))) %>%
  summarise(amount = sum(amount))

base_month <- ymd("20170101")
span_train <- 12
span_test <- 6
slide_size <- 6
for (i in 1:3) {
  sta <- base_month + months((i - 1) * slide_size)
  mid <- sta + months(span_train - 1)
  end <- mid + months(span_test)
  sta <- as.character(sta, "%Y%m")
  mid <- as.character(mid, "%Y%m")
  end <- as.character(end, "%Y%m")
  cat(sta, mid, end, "\n")
  assign(paste0("df_train", i), filter(df_receipt_sum, sta <= sales_month, sales_month <= mid))
  assign(paste0("df_test", i), filter(df_receipt_sum, mid < sales_month, sales_month <= end))
}
rm(i, sta, mid, end, base_month, span_train, span_test, slide_size)
rm(df_receipt_sum, list = ls()[grepl("^df_train|df_test", ls())])


## P-091 ##
# 顧客データフレーム（df_customer）の各顧客に対し、売上実績のある顧客数と売上実績のない顧客数が1:1となるように
# アンダーサンプリングで抽出せよ。

# 売上有無を変数化
tmp <-
  left_join(df_customer,
    distinct(df_receipt, customer_id, .keep_all = T),
    by = "customer_id"
  ) %>%
  mutate(has_receipt = if_else(is.na(amount), 0, 1)) %>%
  select(customer_id, has_receipt)
# 少ないほうのグループに合わせる（少ないほうの件数の2倍を抽出件数とする）
tmp2 <- tapply(tmp$customer_id, tmp$has_receipt, length)
if (tmp2[1] < tmp2[2]) {
  rbind(tmp[tmp$has_receipt == 0, ], sample_n(tmp[tmp$has_receipt == 1, ], size = tmp2[1])) %>%
    group_by(has_receipt) %>%
    summarise(count = length(customer_id))
} else {
  rbind(tmp[tmp$has_receipt == 1, ], sample_n(tmp[tmp$has_receipt == 0, ], size = tmp2[2])) %>%
    group_by(has_receipt) %>%
    summarise(count = length(customer_id))
}
# 不要なデータフレームや値を削除
rm(tmp, tmp2)


## P-092 ##
# 顧客データフレーム（df_customer）では、性別に関する情報が非正規化の状態で保持されている。
# これを第三正規化せよ。
select(df_customer, -gender) %>% str()
distinct(select(df_customer, c(gender_cd, gender))) %>% arrange(gender_cd)


## P-093 ##
# 商品データフレーム（df_product）では各カテゴリのコード値だけを保有し、カテゴリ名は保有していない。
# カテゴリデータフレーム（df_category）と組み合わせて非正規化し、カテゴリ名を保有した新たな
# 商品データフレームを作成せよ。
df_product %>%
  inner_join(select(df_category, c(-category_major_cd, -category_medium_cd)),
    by = "category_small_cd"
  ) %>%
  str()


## P-094 ##
# 先に作成したカテゴリ名付き商品データを以下の仕様でファイル出力せよ。
# なお、出力先のパスはdata配下とする。
#  ・ファイル形式はCSV（カンマ区切り）
#  ・ヘッダ有り
#  ・文字コードはUTF-8
df_product %>%
  inner_join(select(df_category, c(-category_major_cd, -category_medium_cd)),
    by = "category_small_cd"
  ) %>%
  write.csv("./output/P094R.csv", row.names = F, fileEncoding = "utf8")


## P-095 ##
# 先に作成したカテゴリ名付き商品データを以下の仕様でファイル出力せよ。
# なお、出力先のパスはdata配下とする。
#  ・ファイル形式はCSV（カンマ区切り）
#  ・ヘッダ有り
#  ・文字コードはCP932
df_product %>%
  inner_join(select(df_category, c(-category_major_cd, -category_medium_cd)),
    by = "category_small_cd"
  ) %>%
  write.csv("./output/P095R.csv", row.names = F, fileEncoding = "cp932")


## P-096 ##
# 先に作成したカテゴリ名付き商品データを以下の仕様でファイル出力せよ。なお、出力先のパスはdata配下とする。
#  ・ファイル形式はCSV（カンマ区切り）
#  ・ヘッダ無し
#  ・文字コードはUTF-8
df_product %>%
  inner_join(select(df_category, c(-category_major_cd, -category_medium_cd)),
    by = "category_small_cd"
  ) %>%
  readr::write_csv("./output/P096R.csv", col_names = F)


## P-097 ##
# 先に作成した以下形式のファイルを読み込み、データフレームを作成せよ。
# また、先頭10件を表示させ、正しくとりまれていることを確認せよ。
#  ・ファイル形式はCSV（カンマ区切り）
#  ・ヘッダ有り
#  ・文字コードはUTF-8
read.csv("./output/P094R.csv", encoding = "UTF-8") %>% nrow()
readr::read_csv("./output/P094R.csv", locale = locale(encoding = "utf8"))


## P-098 ##
# 先に作成した以下形式のファイルを読み込み、データフレームを作成せよ。
# また、先頭10件を表示させ、正しくとりまれていることを確認せよ。
#  ・ファイル形式はCSV（カンマ区切り）
#  ・ヘッダ無し
#  ・文字コードはUTF-8
read.csv("./output/P096R.csv", encoding = "UTF-8", header = F) %>% nrow()


## P-099 ##
# 先に作成したカテゴリ名付き商品データを以下の仕様でファイル出力せよ。
# なお、出力先のパスはdata配下とする。
#  ・ファイル形式はTSV（タブ区切り）
#  ・ヘッダ有り
#  ・文字コードはUTF-8
df_product %>%
  inner_join(select(df_category, c(-category_major_cd, -category_medium_cd)),
    by = "category_small_cd"
  ) %>%
  readr::write_tsv("./output/P099R.tsv")


## P-100 ##
# 先に作成した以下形式のファイルを読み込み、データフレームを作成せよ。
# また、先頭10件を表示させ、正しくとりまれていることを確認せよ。
#  ・ファイル形式はTSV（タブ区切り）
#  ・ヘッダ有り
#  ・文字コードはUTF-8
read_tsv("./output/P099R.tsv", locale = (locale(encoding = "utf8"))) %>% nrow()
