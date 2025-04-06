# Tidyverse paketini y??kleme
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# Veri setini y??kleme
books_data <- read.csv("https://raw.githubusercontent.com/luminati-io/Amazon-popular-books-dataset/refs/heads/main/Amazon_popular_books_dataset.csv")

# Veri setini ke??fetme
summary(books_data)
str(books_data)

# ??lk birka?? sat??r?? g??r??nt??leme
head(books_data)

# Rating s??tununu say??sal de??ere d??n????t??rme
books_data <- books_data %>%
  mutate(avg_rating = as.numeric(str_extract(rating, "\\d+\\.\\d+")))

# Eksik de??erleri tespit etme
colSums(is.na(books_data))

# Eksik de??erleri temizleme
books_clean <- books_data %>%
  filter(!is.na(avg_rating) & !is.na(reviews_count) & !is.na(final_price))

# Temizlenmi?? veri setini kontrol etme
summary(books_clean)

# Kitap kategorilerine g??re ortalama puanlar?? analiz etme
# Kategoriler JSON format??nda oldu??u i??in ??nce temizleyelim
books_clean <- books_clean %>%
  mutate(main_category = str_extract(categories, "\"([^\"]*)\"", group = 1))

category_ratings <- books_clean %>%
  group_by(main_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n()) %>%
  filter(count > 10) %>%  # En az 10 kitap olan kategorileri filtrele
  arrange(desc(avg_rating))

print(category_ratings)

# En ??ok inceleme alan kitaplar?? belirleme
top_reviewed_books <- books_clean %>%
  arrange(desc(reviews_count)) %>%
  select(title, reviews_count, avg_rating) %>%
  head(10)

print(top_reviewed_books)

# Fiyat ve inceleme say??s?? aras??ndaki ili??kiyi inceleme
# Fiyat kategorileri olu??turma
books_clean <- books_clean %>%
  mutate(price_category = case_when(
    final_price < 10 ~ "Low",
    final_price < 20 ~ "Medium",
    TRUE ~ "High"
  ))

# Korelasyon analizi
cor_value <- cor(books_clean$final_price, books_clean$reviews_count, use = "complete.obs")
print(paste("Fiyat ve inceleme say??s?? aras??ndaki korelasyon:", round(cor_value, 3)))

# Bulgular?? g??rselle??tirme
# ??nceleme say??s?? ve ortalama puan ili??kisi
ggplot(books_clean, aes(x = reviews_count, y = avg_rating)) +
  geom_point(aes(color = price_category), alpha = 0.6) +
  scale_x_log10() +  # ??nceleme say??s?? ??ok geni?? bir aral??kta oldu??u i??in logaritmik ??l??ek kullan??yoruz
  labs(title = "??nceleme Say??s?? ve Ortalama Puan ??li??kisi",
       x = "??nceleme Say??s?? (log ??l??ek)", y = "Ortalama Puan") +
  theme_minimal()

# Kategori baz??nda ortalama puanlar 
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

# Alt kategorilere g??re analiz
category_ratings <- books_clean %>%
  filter(!is.na(sub_category)) %>%
  group_by(sub_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n()) %>%
  filter(count > 10) %>%
  arrange(desc(avg_rating))

# En y??ksek puanl?? 10 alt kategori
top_categories <- category_ratings %>% head(10)

ggplot(top_categories, aes(x = reorder(sub_category, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "#2aa4a9") +
  coord_flip() +
  labs(title = "En Y??ksek Puanl?? 10 Alt Kategori",
       x = "Alt Kategori", y = "Ortalama Puan") +
  theme_minimal()



# Fiyat ve inceleme say??s?? ili??kisi
ggplot(books_clean, aes(x = final_price, y = reviews_count)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_log10() +  # ??nceleme say??s?? i??in logaritmik ??l??ek
  labs(title = "Fiyat ve ??nceleme Say??s?? ??li??kisi",
       x = "Fiyat ($)", y = "??nceleme Say??s?? (log ??l??ek)") +
  theme_minimal()

# Fiyat kategorilerine g??re ortalama puanlar
price_ratings <- books_clean %>%
  group_by(price_category) %>%
  summarise(avg_rating = mean(avg_rating),
            count = n())

ggplot(price_ratings, aes(x = price_category, y = avg_rating, fill = price_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Fiyat Kategorilerine G??re Ortalama Puanlar",
       x = "Fiyat Kategorisi", y = "Ortalama Puan") +
  theme_minimal()
