# Module 3 - Exercise 

## 1. Description

In this exercise, we will use Sentinel-2 Multispectral Instrument, Level-1C Image Collection,
compute the following indices using the training area of interest in Google Earth Engine:
-  Normalized Difference Water Index (NDWI)<br>
   (G - NIR) / (G + NIR), Band 3 (Green) and Band 8 (NIR)
- Normalized Difference Built-up Index (NDBI)<br>
  (SWIR – NIR) / (SWIR + NIR), Band 8 (NIR) and Band 12 (SWIR)
-  Enhanced Vegetation Index (EVI)<br>
  2.5 * ((NIR-RED) / (NIR +6 * RED -7.5* BLUE+1)), Band 8 (NIR), Band 4 (Red), Band 2 (Blue).



## 2. Skills Practices

This exercise covers:

- Exploring and calculating Spectral indices in Google Earth Engine




## 3. Instruction

1. Import the Sentinel 2 MSI, Level-1C Collection and the coordinate **(0.03695166, 7.12756658)** as your geometry. Add point to map and center your map to the point geometry.
2. Set up the mask function to mask clouds using the Sentinel-2 QA band, clouds and cirrus bands.
 **NB: Use the function below;**

    - [Function to mask clouds using the Sentinel-2 QA band.](https://code.earthengine.google.com/2e3d32e58da17f5610d5256b64a280a3?noload=true)

3. Map the function over one year (2019) of data and take the median, bounded to the point of interest and visualize the image in the natural color composite ( B4, B3, B2)

    - [ Map the function over a year's  data  taking the median,](https://code.earthengine.google.com/d472d85f373ad7520803a47aea3f79ea?noload=true)

4. Compute the indices

    - Normalized Difference Water Index (NDWI)<br>
     (G - NIR) / (G + NIR), Band 3 (Green) and Band 8 (NIR)<br>
    - Normalized Difference Built-up Index (NDBI)<br>
    (SWIR – NIR) / (SWIR + NIR), Band 8 (NIR) and Band 12 (SWIR)<br>
    - Enhanced Vegetation Index (EVI)<br>
    2.5 * ((NIR-RED) / (NIR +6 * RED -7.5* BLUE+1)), Band 8 (NIR), Band 4 (Red), Band 2 (Blue).<br>

5. Add indices to map visualize them with or without color palettes.






> :pushpin: Expected Results <br>

<table style="border: 0;">  
  <tr> 
    <td vlign="center" style="border: 0;"><img src="https://github.com/ernest19/UKPACT_GEE_TRAINING/blob/main/img/exercise/mod3_exercise1.png" width="400"></td>
  </tr>
</table>


<table style="border: 0;">  
  <tr> 
    <td vlign="center" style="border: 0;"><img src="https://github.com/ernest19/UKPACT_GEE_TRAINING/blob/main/img/exercise/mod3_exercise11.png" width="400"><br> Natural color composite</td>

  <td vlign="center" style="border: 0;"><img src="https://github.com/ernest19/UKPACT_GEE_TRAINING/blob/main/img/exercise/mod3_exercise12.png" width="400"><br> NDWI Output
  </td>
  </tr>

   <tr> 
    <td vlign="center" style="border: 0;"><img src="https://github.com/ernest19/UKPACT_GEE_TRAINING/blob/main/img/exercise/mod3_exercise13.png" width="400"><br> NDBI Output
</td>

  <td vlign="center" style="border: 0;"><img src="https://github.com/ernest19/UKPACT_GEE_TRAINING/blob/main/img/exercise/mod3_exercise14.png" width="400"><br>  EVI Output 
  </td>
  </tr>
</table>


###   :pushpin: Submit Exercise 1 <br>

 Please submit your exercises [here](https://github.com/ernest19/UKPACT_GEE_TRAINING/issues/new?assignees=&labels=&template=submit-module-3-exercises.md&title=Module+3+exercises1+%5Breplace+with+your+name%5D)




