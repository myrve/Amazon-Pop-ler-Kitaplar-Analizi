# Tidyverse paketini yukleme
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# Veri setini yukleme
books_data <- read.csv("https://raw.githubusercontent.com/luminati-io/Amazon-popular-books-dataset/refs/heads/main/Amazon_popular_books_dataset.csv")

# Veri setini kesfetme
summary(books_data)
str(books_data)

# ilk birkac satiri goruntuleme
head(books_data)

# Rating sutununu sayısal degere donusturme
books_data <- books_data %>%
  mutate(avg_rating = as.numeric(str_extract(rating, "\\d+\\.\\d+")))

# Eksik degerleri tespit etme
colSums(is.na(books_data))

# Eksik degerleri temizleme
books_clean <- books_data %>%
  filter(!is.na(avg_rating) & !is.na(reviews_count) & !is.na(final_price))

# Temizlenmis veri setini kontrol etme
summary(books_clean)

# Kitap kategorilerine gore ortalama puanlari analiz etme
# Kategoriler JSON formatinda oldugu icin once temizleyelim
books_clean <- books_clean %>%
  mutate(main_category = str_extract(categories, "\"([^\"]*)\"", group = 1))

category_ratings <- books_clean %>%
  group_by(main_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n()) %>%
  filter(count > 10) %>%  # En az 10 kitap olan kategorileri filtrele
  arrange(desc(avg_rating))

print(category_ratings)

# En cok inceleme alan kitaplari belirleme
top_reviewed_books <- books_clean %>%
  arrange(desc(reviews_count)) %>%
  select(title, reviews_count, avg_rating) %>%
  head(10)

print(top_reviewed_books)

# Fiyat ve inceleme sayisi arasindaki iliskiyi inceleme
# Fiyat kategorileri olusturma
books_clean <- books_clean %>%
  mutate(price_category = case_when(
    final_price < 10 ~ "Low",
    final_price < 20 ~ "Medium",
    TRUE ~ "High"
  ))

# Korelasyon analizi
cor_value <- cor(books_clean$final_price, books_clean$reviews_count, use = "complete.obs")
print(paste("Fiyat ve inceleme sayisi arasindaki korelasyon:", round(cor_value, 3)))

# Bulgulari gorsellestirme
# inceleme sayisi ve ortalama puan iliskisi
ggplot(books_clean, aes(x = reviews_count, y = avg_rating)) +
  geom_point(aes(color = price_category), alpha = 0.6) +
  scale_x_log10() +  # Inceleme sayisi cok genis bir aralikta oldugu icin logaritmik olcek kullaniyoruz
  labs(title = "inceleme sayisi ve ortalama puan iliskisi",
       x = "inceleme sayisi (log olcek)", y = "Ortalama Puan") +
  theme_minimal()

# Kategori bazinda ortalama puanlar 
books_clean <- books_clean %>%
  mutate(
    categories_parsed = lapply(categories, function(x) {
      tryCatch({
        fromJSON(x)
      }, error = function(e) {
        NULL
      })
    }),
    sub_category = sapply(categories_parsed, function(x) {
      if(length(x) > 1) x[2] else NA
    })
  )

# Alt kategorilere gore analiz
category_ratings <- books_clean %>%
  filter(!is.na(sub_category)) %>%
  group_by(sub_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(desc(avg_rating))

# En yuksek puanli 10 alt kategori
top_categories <- category_ratings %>% head(10)

ggplot(top_categories, aes(x = reorder(sub_category, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "#2aa4a9") +
  coord_flip() +
  labs(title = "En Yuksek Puanli 10 Alt Kategori",
       x = "Alt Kategori", y = "Ortalama Puan") +
  theme_minimal()



# Fiyat ve inceleme sayisi iliskisi
ggplot(books_clean, aes(x = final_price, y = reviews_count)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_log10() +  # Inceleme sayisi icin logaritmik olcek
  labs(title = "Fiyat ve Inceleme Sayisi Iliskisi",
       x = "Fiyat ($)", y = "Inceleme Sayisi (log olcek)") +
  theme_minimal()

# Fiyat kategorilerine gore ortalama puanlar
price_ratings <- books_clean %>%
  group_by(price_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n())

ggplot(price_ratings, aes(x = price_category, y = avg_rating, fill = price_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Fiyat Kategorilerine Gore Ortalama Puanlar",
       x = "Fiyat Kategorisi", y = "Ortalama Puan") +
  theme_minimal()
