source("function.R")
df <- read_csv("Visualization.csv")
# interval visualization
his_monthly_rm <- ggplot(data = df, aes(x = df$monthly_rent_rm)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(title = "Monthly Rent Distribution", x = "Monthly Rent (RM)", y = "Frequency") +
  theme_minimal() +
  xlim(100, 5000)

his_size <- ggplot(data = df, aes(x = df$size_sqft)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(title = "Size sqft Distribution", x = "Size Sqft", y = "Frequency") +
  theme_minimal() +
  xlim(100, 2000)

his_completion_year <- ggplot(data = df, aes(x = df$completion_year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Completion Year Distribution", x = "Completion Year", y = "Frequency") +
  theme_minimal() +
  xlim(2010, 2025)

his_rooms <- ggplot(data = df, aes(x = df$rooms)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Rooms Distribution", x = "Rooms", y = "Frequency") +
  theme_minimal() +
  xlim(1, 5)

his_parking <- ggplot(data = df, aes(x = df$parking)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Parking count Distribution", x = "Parking", y = "Frequency") +
  theme_minimal() +
  xlim(0, 4)

his_bathroom <- ggplot(data = df, aes(x = df$bathroom)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Bathroom Distribution", x = "Bathroom", y = "Frequency") +
  theme_minimal() +
  xlim(0, 4)

his_fac_count <- ggplot(data = df, aes(x = df$fac_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Facility Distribution", x = "Facilities count", y = "Frequency") +
  theme_minimal() 

his_add_fac_count <- ggplot(data = df, aes(x = df$add_fac_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Additional Facility Distribution", x = "Additional Facilities count", y = "Frequency") +
  theme_minimal() 

# boxplot 
# Create boxplot with uncropped rental data
box_uncropped_rent <- ggplot(df, aes(x = "Monthly Rent (RM)", y = monthly_rent_rm)) +
  geom_boxplot() +
  ylim(0, 20000) +
  labs(x = NULL, y = "Monthly Rent (RM)") +
  ggtitle("Monthly Rent (RM)")
# Create boxplot with rental data cropped at 5,000 RM
box_cropped_rent <- ggplot(df, aes(x = "Monthly Rent (RM)", y = monthly_rent_rm)) +
  geom_boxplot() +
  ylim(100, 4000) +
  labs(x = NULL, y = "Monthly Rent (RM)") +
  ggtitle("Cropped at 100-4,000 RM")

box_uncropped_size <- ggplot(df, aes(x = "Size (sqft)", y = size_sqft)) +
  geom_boxplot() +
  ylim(0, 20000) +
  labs(x = NULL, y = "Size (sqft)") +
  ggtitle("Size (sqft)")
box_cropped_size <- ggplot(df, aes(x = "Size (sqft)", y = size_sqft)) +
  geom_boxplot() +
  ylim(100, 2000) +
  labs(x = NULL, y = "Size (sqft)") +
  ggtitle("Cropped at 100-2,000 square feet")

box_uncropped_rooms <- ggplot(df, aes(x = "Rooms", y = rooms)) +
  geom_boxplot() +
  ylim(0, 10) +
  labs(x = NULL, y = "Rooms") +
  ggtitle("Rooms")
box_cropped_rooms <- ggplot(df, aes(x = "Rooms", y = rooms)) +
  geom_boxplot() +
  ylim(1, 5) +
  labs(x = NULL, y = "Rooms") +
  ggtitle("Cropped at 1-5 Rooms")

box_uncropped_parking <- ggplot(df, aes(x = "Parking", y = parking)) +
  geom_boxplot() +
  ylim(0, 10) +
  labs(x = NULL, y = "Parking") +
  ggtitle("Parking")
box_cropped_parking <- ggplot(df, aes(x = "Parking", y = parking)) +
  geom_boxplot() +
  ylim(0, 4) +
  labs(x = NULL, y = "Parking") +
  ggtitle("Cropped at 0-4 Parking")

box_uncropped_bathrooms <- ggplot(df, aes(x = "Bathrooms", y = bathroom)) +
  geom_boxplot() +
  ylim(0, 10) +
  labs(x = NULL, y = "Bathrooms") +
  ggtitle("Bathrooms")
box_cropped_bathrooms <- ggplot(df, aes(x = "Bathrooms", y = bathroom)) +
  geom_boxplot() +
  ylim(1, 4) +
  labs(x = NULL, y = "Bathrooms") +
  ggtitle("Cropped at 1-4 Bathrooms")

# interval correlation
line_size_VS_rent <- ggplot(df, aes(x = size_sqft, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(50, 3000) +
  labs(x = "Size (sqft)", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Size vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_year_VS_rent <- ggplot(df, aes(x = completion_year, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(2010, 2025) +
  labs(x = "completion_year", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Year vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_rooms_VS_rent <- ggplot(df, aes(x = rooms, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(1, 5) +
  labs(x = "Rooms", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Rooms vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_bathroom_VS_rent <- ggplot(df, aes(x = bathroom, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(1, 4) +
  labs(x = "Bathroom", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Bathroom vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_Parking_VS_rent <- ggplot(df, aes(x = parking, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(1, 4) +
  labs(x = "Parking", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Parking vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_fac_count_VS_rent <- ggplot(df, aes(x = fac_count, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(0, 15) +
  labs(x = "Fac_count", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Fac_count vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

line_add_fac_count_VS_rent <- ggplot(df, aes(x = add_fac_count, y = monthly_rent_rm)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE)+
  ylim(100, 5000) +
  xlim(0, 6) +
  labs(x = "Add_fac_count", y = "Monthly Rent (RM)") +
  ggtitle("Scatter Plot: Add_fac_count vs Monthly Rent") +
  theme(plot.title = element_text(hjust = 0.4))

# nominal corr
box_lrt_VS_rent <- ggplot(data = df, aes(x = monthly_rent_rm, y = nearby_ktm_lrt)) +
  geom_boxplot() +
  xlim(0, 4000) +
  labs(title = "Boxplot", x = "Monthly Rent (RM)", y = "Nearby Railways")

box_property_VS_rent <- ggplot(data = df, aes(x = monthly_rent_rm, y = property_type)) +
  geom_boxplot() +
  xlim(0, 4000) +
  labs(title = "Boxplot", x = "Monthly Rent (RM)", y = "Property type")

box_furnished_VS_rent <- ggplot(data = df, aes(x = monthly_rent_rm, y = furnished)) +
  geom_boxplot() +
  xlim(0, 4000) +
  labs(title = "Boxplot", x = "Monthly Rent (RM)", y = "Furnished")

box_region_VS_rent <- ggplot(data = df, aes(x = monthly_rent_rm, y = region)) +
  geom_boxplot() +
  xlim(0, 4000) +
  labs(title = "Boxplot", x = "Monthly Rent (RM)", y = "Region")

box_location_VS_rent <- ggplot(data = df, aes(x = monthly_rent_rm, y = location)) +
  geom_boxplot() +
  xlim(0, 4000) +
  labs(title = "Boxplot", x = "Monthly Rent (RM)", y = "Location")

bar_charts <- list()

col <- c("completion_year", "property_type", "rooms", "parking", "bathroom",
                  "size_sqft", "furnished", "region", "fac_count", "add_fac_count",
                  "nearby_ktm_lrt","monthly_rent_rm")
# Loop through each variable and create a bar chart
for (variable in col) {
  # Create a bar chart for the current variable
  chart <- ggplot(data = df, aes(x = factor(.data[[variable]], levels = unique(.data[[variable]])))) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = variable, x = variable, y = "Frequency") +
    scale_x_discrete(drop = FALSE) +
    theme_minimal()
  
  # Add the bar chart to the list
  bar_charts[[variable]] <- chart
}





