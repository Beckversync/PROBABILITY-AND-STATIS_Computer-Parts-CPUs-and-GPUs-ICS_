# BÀI TẬP LỚN XÁC SUẤT THỐNG KÊ

## Đọc và trích xuất dữ liệu

Đọc file dữ liệu:

```{r}

df <- read.csv("All_GPUs.csv")
head(df)
View(df)
```

Trích xuất các mẫu cần thiết từ dữ liệu đã đọc:

```{r message=FALSE, warning=FALSE}

df <- df[, c("Name", 
             "Best_Resolution", 
             "Core_Speed", 
             "Max_Power", 
             "Memory",
             "Memory_Bandwidth", 
             "Memory_Speed", 
             "Manufacturer", 
             "Release_Date", 
             "Release_Price")]
head(df)
View(df)
```

## Tiền xử lí số liệu

**Phân tách, chuyển đổi đơn vị:**

Khai báo thư viện **tidyr** sử dụng cho việc định dạng dữ liệu

```{r}

library(tidyr)
```

Đối với mẫu **Name**: Giữ lại từ đầu tiên:

```{r}

# Tách và giữ lại từ đầu tiên của các phần tử trong cột Name
df$Name <- sapply(strsplit(df$Name, " "), "[", 1)
```

Đối với mẫu **Best_Resolution**:

-   Tính số điểm ảnh (pixels) bằng cách thực hiện phép nhân giữa số pixel theo nhiều ngang và chiều dọc.

-   Đồng thời, đổi tên mẫu này thành **Number_of_Pixels**.

```{r}

# Đổi tên Best_Resolution thành Number_of_Pixels
names(df)[names(df) == "Best_Resolution"] <- "Number_of_Pixels"

# Thực hiện phép nhân số pixel hai chiều ngang và dọc
df$Number_of_Pixels <- sapply(strsplit(df$Number_of_Pixels, " x "), 
                       function(x) as.numeric(x[1]) * as.numeric(x[2]))
```

Đối với các mẫu **Core_Speed**, **Max_Power**, **Memory**, **Memory_Bandwidth**, **Memory_Speed**: Tách mỗi mẫu thành hai cột, phần giá trị và phần đơn vị

```{r}

# Do cột Core_Speed có các giá trị khuyết biểu diễn bởi "-" nên cần loại bỏ trước khi xử lý
df$Core_Speed[grepl("-", df$Core_Speed)] <- ""

# Tách cột Core_Speed thành hai cột Core_Speed_Value và Core_Speed_Unit 
df <- separate(df, col = Core_Speed, into = c("Core_Speed_Value", "Core_Speed_Unit"), 
               sep = " ", fill = "right")

# Chuyển cột Core_Speed_Value về định dạng số
df$Core_Speed_Value <- as.numeric(df$Core_Speed_Value)

####

#Thực hiện tương tự với các cột còn lại
df <- separate(df, col = Max_Power, into = c("Max_Power_Value", "Max_Power_Unit"), 
               sep = " ", fill = "right")

df$Max_Power_Value <- as.numeric(df$Max_Power_Value)

###

df <- separate(df, col = Memory, into = c("Memory_Value", "Memory_Unit"), 
               sep = " ", fill = "right", extra = "drop")

df$Memory_Value <- as.numeric(df$Memory_Value)

###

df <- separate(df, col = Memory_Bandwidth, into = c("Memory_Bandwidth_Value", "Memory_Bandwidth_Unit"),
               sep = "(?<=\\d)(?=[A-Za-z])", fill = "right")

df$Memory_Bandwidth_Value <- as.numeric(df$Memory_Bandwidth_Value)

###

df <- separate(df, col = Memory_Speed, into = c("Memory_Speed_Value", "Memory_Speed_Unit"),
               sep = " ", fill = "right")

df$Memory_Speed_Value <- as.numeric(df$Memory_Speed_Value)
```

Kiểm tra đơn vị của các thuộc tính

```{r}

table(df$Core_Speed_Unit)
table(df$Max_Power_Unit)
table(df$Memory_Unit)
table(df$Memory_Bandwidth_Unit)
table(df$Memory_Speed_Unit)
```

Đồng nhất đơn vị của **Memory_Bandwidth**

```{r}

# Chia giá trị của các phần tử có đơn vị [MB/sec] cho 1024 để đưa về đơn vị [GB/sec] 
df$Memory_Bandwidth_Value <- ifelse(df$Memory_Bandwidth_Unit == "MB/sec", 
                                    df$Memory_Bandwidth_Value / 1024,
                                    df$Memory_Bandwidth_Value)

# Thay đổi đơn vị của các phần tử đã chuyển giá trị thành [GB/sec]
df$Memory_Bandwidth_Unit <- ifelse(df$Memory_Bandwidth_Unit == "MB/sec",
                                   "GB/sec", df$Memory_Bandwidth_Unit)

# Kiểm tra việc chuyển đổi
table(df$Memory_Bandwidth_Unit)
```

Đối với mẫu **Release_Date**:

-   Chuyển về định dạng ngày.

-   Tính lại giá trị theo công thức: **Release_Date = năm + tháng/12**

```{r}

# Chuyển về định dạng ngày tháng
df$Release_Date   <-  as.Date(sub("^\\s*\\n", "", df$Release_Date), format="%d-%b-%Y")

# Tính giá trị của các phần tử theo công thức
df$Release_Date   <-  as.numeric(format(df$Release_Date, "%Y"))  +
                      as.numeric(format(df$Release_Date, "%m")) / 12
```

Đối với mẫu **Release_Price**:

-   Xoá bỏ đơn vị

-   Chuyển về định dạng số

```{r}

# Xoá đi ký tự $ và dấu phẩy
df$Release_Price <- gsub("[$,]", "", df$Release_Price)

# Chuyển về định dạng số
df$Release_Price <- as.numeric(df$Release_Price)
```

Kiểm tra lại toàn bộ quá trình xử lý định dạng

```{r}

# Xem kiểu dữ liệu của các cột
str(df)

# Xem một vài phần tử đầu tiên của dữ liệu
View(df)
```

## Xử lý dữ liệu khuyết

Kiểm tra số lượng và tỉ lệ khuyết dữ liệu của các mẫu

```{r}

# Trích các mẫu cần kiểm tra
sub_df <- df[, c("Name", 
                 "Number_of_Pixels", 
                 "Core_Speed_Value", 
                 "Max_Power_Value", 
                 "Memory_Value", 
                 "Memory_Bandwidth_Value",
                 "Memory_Speed_Value",
                 "Manufacturer",
                 "Release_Date",
                 "Release_Price")] 

# Đếm số lượng phẩn tử bị khuyết dữ liệu của các mẫu
apply(is.na(sub_df), MARGIN = 2, FUN = sum)

# Tính tỉ lệ khuyết dữ liệu của các mẫu
apply(is.na(sub_df), MARGIN = 2, FUN = mean)
```

Xoá bỏ đi các quan sát bị khuyết dữ liệu tạm thời bỏ qua thuộc tính Release_Price

```{r}

# Bỏ qua Release_Price trong quá trình xoá quan sát khuyết
sub_df <- df[, !(names(df) %in% "Release_Price")]

# Xoá các hàng khuyết dữ liệu
sub_df <- na.omit(sub_df)

# Gộp lại dữ liệu đã xử lí khuyết vào tập dữ liệu ban đầu
df <- df[rownames(sub_df), ]


# Kiểm tra lại tỉ lệ khuyết trong tập dữ liệu
apply(is.na(df), MARGIN = 2, FUN = mean)

# Xem kết quả xử lý
View(df)
```

## Thống kê tả

Các giá trị đặc trưng của mẫu

```{r}

# Trích xuất các mẫu dữ liệu định lượng
qtt_df  <-  df[,  c("Number_of_Pixels",  
                    "Core_Speed_Value",
                    "Max_Power_Value", 
                    "Memory_Value", 
                    "Memory_Bandwidth_Value",
                    "Memory_Speed_Value",
                    "Release_Date")]

# Tính trung bình của từng mẫu
mean <- apply(qtt_df, 2, mean)
# Tính phương sai mẫu (hiệu chỉnh)
s2   <- apply(qtt_df, 2, var)
# Tính các điểm tứ phân vị
Q1 <- apply(qtt_df, 2, function(x) quantile(x, probs=0.25,na.rm = TRUE))
Q2 <- apply(qtt_df, 2, function(x) quantile(x, probs=0.50,na.rm = TRUE))
Q3 <- apply(qtt_df, 2, function(x) quantile(x, probs=0.75,na.rm = TRUE))
# Tính giá trị nhỏ nhất
min  <- apply(qtt_df, 2, min)
# Tính giá trị lớn nhất
max  <- apply(qtt_df, 2, max)
# In các đặc trưng theo dạng bảng, làm tròn 2 chữ số thập phân
round(data.frame(mean, s2, Q1, Q2, Q3, min, max), 2)  
```

Phân phối tần số

```{r}

# Hàm tự định nghĩa để vẽ biểu đồ tần số của một mẫu sample
plotHist <- function(sample, name) {
  
  # Thiết lại lại khoảng chia trên trục tung
  x_axis = seq(floor(min(sample)), max(sample)*1.1, by = floor((max(sample)*1.1-min(sample))/20))
  
  # Hàm vẽ biểu đồ tần số của mẫu sample
  hist(sample, main="", xlab=name, xaxt="n", ylab="Tần số", labels=TRUE, breaks=x_axis)

    # Hiện trục tung với khoảng chia đã thiết lập
  axis(1, at = x_axis)
}
```

Lần lượt dùng hàm đã định nghĩa để vẽ biểu đồ tần số của một vài mẫu

```{r}

plotHist(df$Core_Speed_Value,       name = "Core Speed [MHz]")
```

```{r}

plotHist(df$Max_Power_Value,        name = "Max Power [Watts]")
```

```{r}

plotHist(df$Memory_Bandwidth_Value, name = "Memory Bandwidth [GB/sec]")
```

```{r}

plotHist(df$Memory_Speed_Value,     name = "Memory Speed [MB]")
```

Phân phối chuẩn

```{r message=FALSE, warning=FALSE}

library(ggpubr)
```

```{r}

ggqqplot(df$Core_Speed_Value,       ylab = "Core Speed [MHz]")
```

```{r}

ggqqplot(df$Max_Power_Value,        ylab = "Max Power [Watts]")
```

```{r}

ggqqplot(df$Memory_Bandwidth_Value, ylab = "Memory Bandwidth [GB/sec]")
```

```{r}
ggqqplot(df$Memory_Speed_Value,     ylab = "Memory Speed [MHz]")
```

Mối liên hệ giữa các biến

```{r}

# Trích các mẫu
qtt_df <- df[, c("Number_of_Pixels", 
                 "Core_Speed_Value", 
                 "Max_Power_Value", 
                 "Memory_Value", 
                 "Memory_Speed_Value", 
                 "Memory_Bandwidth_Value",
                 "Release_Date")]

# Hàm pairs() tạo ma trận các biểu đồ tán xạ giữa các mẫu theo cặp
pairs(qtt_df, main="", pch=20)
```

## Thống kê suy diễn

### Tìm khoảng tin cậy một mẫu

```{r}

# Trích xuất mẫu Core_Speed_Value từ tập dữ liệu
sample <- df$Core_Speed_Value

# Độ tin cậy = 1 - alpha => alpha = 1 - Độ tin cậy
alpha  <-  1  -  0.95

# Áp dụng t-test cho mẫu sample
t.test(sample, conf.level = 1 - alpha)
```

### Kiểm định hai mẫu

Thống kê số lượng GPU của mỗi nhãn hàng có trong tập dữ liệu

```{r}

table(df$Manufacturer)
```

Trích xuất hai mẫu con từ Core_Speed_Value theo Manufacturer là "AMD" và "Nvidia"

```{r}

sample1 <- df$Core_Speed_Value[df$Manufacturer == "AMD"]
sample2 <- df$Core_Speed_Value[df$Manufacturer == "Nvidia"]
```

Kiểm định trung bình hai mẫu con vừa trích xuất

```{r}

# Mức ý nghĩa của kiểm định, cho trước = 5%
alpha <- 0.05

# Dùng hàm t.test() để kiểm định trung bình cho hai mẫu
t.test(sample1,  sample2,  var.equal = (var(sample1)  ==  var(sample2)),
       conf.level = 1 - alpha, alternative = "two.sided")
```

### Phân tích phương sai một yếu tố (one-way ANOVA)

Thống kê số lượng quan sát tương ứng với từng tên trong Name

```{r}

# Lưu kết quả của hàm table() vào biến và in kết quả đó ra
print(name_table <- table(df$Name))
```

Lọc bỏ các quan sát có số lượng Name không lớn hơn 10

```{r}

# Trích xuất dữ liệu với số lượng mỗi quan sát theo tên nhiều hơn 10
df_filtered <- df[df$Name %in% names(name_table[name_table > 10]), ]

# Kiểm tra lại thống kê các tên
table(df_filtered$Name)
```

Kiểm định sự tương đồng của phương sai các nhóm

```{r}

bartlett.test(Memory_Speed_Value ~ Name, data = df_filtered)
```

Tiến hành phân tích phương sai

```{r}

# Lưu kết quả của mô hình ANOVA vào biến (để sử dụng lại cho sau này)
anova_result <- aov(Memory_Speed_Value ~ Name, data = df_filtered)

# Hiển thị kết quả chi tiết của mô hình ANOVA
summary(anova_result)
```

Phân tích sâu: kiểm định giả thuyết về trung bình Memory_Speed giữa từng nhóm.

```{r message=FALSE, warning=FALSE}

library(agricolae)

# Dùng LSD.test() cho kết quả của mô hình ANOVA để kiểm định trung bình giữa các nhóm 
print(LSD.test(anova_result, "Name", alpha = 0.05))
```

Dùng thêm hàm pairwise.t.test() để xem ma trận các p-value của kiểm định Student theo cặp với độ lệch chuẩn gộp giữa các nhóm.

```{r}

pairwise.t.test(df_filtered$Memory_Speed_Value, df_filtered$Name, p.adjust.method = "none")
```

### Hồi quy tuyến tính

Trích ra các mẫu định lượng trong tập dữ liệu Lọc lấy những quan sát có yếu tố Release_Price (bước đã tạm thời bỏ qua trong phần tiền xử lý)

```{r}

# Trích các mẫu định lượng
samples <- df[ , c("Release_Price", "Number_of_Pixels",
                   "Core_Speed_Value", "Max_Power_Value", 
                   "Memory_Value", "Memory_Bandwidth_Value", 
                   "Memory_Speed_Value", "Release_Date")]
                   
# Giữ lại những dòng có giá trị Release_Price
samples <- samples[!is.na(samples$Release_Price), ]

# Xem dữ liệu sau khi trích xuất và xử lý
View(samples)
```

**\*Xây dựng mô hình**

```{r}

# Dấu . thể hiện các biến còn lại trong samples
lm_result <- lm(Release_Price ~ ., data = samples)

summary(lm_result)
```

Xây dựng mô hình hồi quy tuyến tính với các biến có nghĩa

```{r}

lm_result <- lm(Release_Price ~ Max_Power_Value + Memory_Value +
                Memory_Bandwidth_Value + Memory_Speed_Value, data = samples)

summary(lm_result)
```

Do hệ số chặn không có ý nghĩa, cần loại bớt đi biến độc lập có p-value cao nhất, ở đây là Memory_Speed, và xây dựng lại mô hình một lần nữa.

```{r}

lm_result <- lm(Release_Price ~ Max_Power_Value + Memory_Value + 
                Memory_Bandwidth_Value, data = samples)

summary(lm_result)
```

Ước lượng khoảng tin cậy cho các hệ số

```{r}

# Độ tin cậy của khoảng ước lượng
alpha = 0.05
# Tìm khoảng ước lượng cho các hệ số  
confint(lm_result, level = 1 - alpha)
```

**\*Kiểm tra giả thiết mô hình**

vẽ biểu đồ phần dư để kiểm tra các giả thiết về phần dư trên cho mô hình đã xây dựng

```{r}

# Thiết lập vị trí các biểu đồ trên cửa sổ (hiển thị 4 biểu đồ trên ma trận 2x2)
par(mfrow = c(2, 2))

# Dùng hàm plot() cho kết quả mô hình đã xây dựng
plot(lm_result, pch = 20)
```

Trích xuất phần dư của mô hình, sau đó kiểm định giả thiết sai số ngẫu nhiên có phân phối chuẩn.

```{r}

# Trích xuất các phần dư từ mô hình đã xây dựng
resid <- resid(lm_result)
# Dùng Shapiro-test cho mẫu phần dư đã trích để kiểm định về sai số ngẫu nhiên
shapiro.test(resid)
```

Kiểm định giả thuyết Kỳ vọng của sai số ngẫu nhiên tại mỗi giá trị bằng 0.

```{r}

alpha <- 0.05
t.test(resid, mu = 0, conf.level = 1 - alpha)
```

Kiểm định giả thuyết Phương sai sai số ngẫu nhiên không đổi.

```{r message=FALSE, warning=FALSE}

library(car)

ncvTest(lm_result)
```

Kiểm định tính đa cộng tuyến giữa các biến độc lập.

```{r}

vif(lm_result)
```

**\*Dự đoán**

```{r}

# Khởi tạo các giá trị cần dự đoán cho các biến độc lập
Max_Power_Value        <- c(  200,  225,  150,  190)
Memory_Value           <- c(16000, 8190, 2048, 1024)
Memory_Bandwidth_Value <- c(  220,   70,  110,  150)
new <- data.frame(Max_Power_Value, Memory_Value, Memory_Bandwidth_Value)

# Dự đoán giá trị Release Price tương ứng với giá trị các biến độc lập đã tạo
predict(lm_result, newdata = new, interval = "confidence")
```
