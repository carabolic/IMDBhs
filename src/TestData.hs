{-# LANGUAGE OverloadedStrings #-}

module TestData where

import Data.Text (Text)

seriesData :: Text
seriesData = "\
  \\"10-nen saki mo kimi ni koishite\" (2010)        2010-????\n\
  \\"1 Equipo\" (2005)                   2005-????\n\
  \\"1 Erkek 1 Kadin\" (2012)                2012-????\n\
  \\"10 Years Younger\" (2004/I)             2004-????\n\
  \\"114\" (????)                        ????\n\
  \"

episodeData :: Text
episodeData = "\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Kimi no iru mirai e (#1.6)}   2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no giseisha? (#1.4)}  2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no honeorizon (#1.3)} 2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no kara sawagi (#1.5)}    2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Mirai kara no koibito (#1.1)} 2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Unmei no koi nante (#1.2)}    2010\n\
  \\"1 Equipo\" (2005) {(2005-11-23)}            2005\n\
  \\"1 Erkek 1 Kadin\" (2012) {(#2.5)}           ????\n\
  \\"10 Years Younger\" (2004/I) {(#2.8)}            2005\n\
  \\"10 Years Younger\" (2004/I) {(#5.3)}            2008\n\
  \\"25 & Counting\" (????) {Pilot: The Quarter-Life Crisis (#1.1)}  ????\n\
  \\"Japan Booze Blind\" (2009) {India Pale Ale}     2013\n\
  \\"Japan Booze Blind\" (2009) {Kyushu (Part I)}        2011\n\
  \\"Japan Booze Blind\" (2009) {Kyushu (Part II)}       2011\n\
  \\"Japan Booze Blind\" (2009) {Kyushu (Part III)}      2011\n\
  \\"Japan Booze Blind\" (2009) {Nippon Craft Beer Festival (Part I)}    2010\n\
  \\"Japan Booze Blind\" (2009) {Nippon Craft Beer Festival (Part II)}   2010\n\
  \\"Japan Booze Blind\" (2009) {Nippon Craft Beer Festival (Part III)}  2010\n\
  \"

movieData :: Text
movieData = "\
  \Abe's Diner (????) {{SUSPENDED}}            ????\n\
  \Abe's Diner (????/II) {{SUSPENDED}}         ????\n\
  \The Moung Young Trio (1901/II)              1901\n\
  \The Matrix Defence (2003) (TV)              2003\n\
  \The Matrix Online (2005) (VG)               2005\n\
  \The Matrix Recalibrated (2004) (V)          2004\n\
  \The Matrix (1999)                   1999\n\
  \The Motel (2012) (V) {{SUSPENDED}}          2012\n\
  \"

moviesListData :: Text
moviesListData = "\
  \\"10-nen saki mo kimi ni koishite\" (2010)        2010-????\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Kimi no iru mirai e (#1.6)}   2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no giseisha? (#1.4)}  2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no honeorizon (#1.3)} 2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Koi no kara sawagi (#1.5)}    2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Mirai kara no koibito (#1.1)} 2010\n\
  \\"10-nen saki mo kimi ni koishite\" (2010) {Unmei no koi nante (#1.2)}    2010\n\
  \\"1 Equipo\" (2005)                   2005-????\n\
  \\"1 Equipo\" (2005) {(2005-11-23)}            2005\n\
  \\"1 Erkek 1 Kadin\" (2012)                2012-????\n\
  \\"1 Erkek 1 Kadin\" (2012) {(#2.5)}           ????\n\
  \\"10 Years Younger\" (2004/I)             2004-????\n\
  \\"10 Years Younger\" (2004/I) {(#2.8)}            2005\n\
  \\"10 Years Younger\" (2004/I) {(#5.3)}            2008\n\
  \\"114\" (????)                        ????\n\
  \\"25 & Counting\" (????) {Pilot: The Quarter-Life Crisis (#1.1)}  ????\n\
  \Abe's Diner (????) {{SUSPENDED}}            ????\n\
  \Abe's Diner (????/II) {{SUSPENDED}}         ????\n\
  \The Moung Young Trio (1901/II)              1901\n\
  \The Matrix Defence (2003) (TV)              2003\n\
  \The Matrix Online (2005) (VG)               2005\n\
  \The Matrix Recalibrated (2004) (V)          2004\n\
  \The Matrix (1999)                   1999\n\
  \The Motel (2012) (V) {{SUSPENDED}}          2012\n\
  \"
