#!/bin/bash


# note - to run using env vars in the running shell,run with `. .updateRelease`
release_name=uiExplorerData
asset_name=unemployment_data.parquet


echo "Updating release $release_name"


function create_release {
  echo "Release not found. Creating."
  new_release=$(curl \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
     https://api.github.com/repos/clsphila/ui-data-explorer/releases \
    -d "{\"tag_name\":\"$release_name\", \"name\":\"$release_name\"}"
    )
  echo "new release: $new_release"
  assets_url=$(jq '.assets_url' <<< $new_release)
  echo "Assets url: $assets_url"
  uploaded_response=$(curl \
    -X POST \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Content-Type: application/octet-stream" \
    "$assets_url" \
    --data-binary @"ui-data-explorer/data/unemployment_data.parquet"
    )
  echo "$uploaded_response"
}

function replace_asset {
  echo "Replacing asset."
}

# get the releases
releases_complete=$(curl \
  -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/clsphila/ui-data-explorer/releases)

echo "all releases: $releases_complete"

# find the release we care about with its name.
releases=$(jq --arg release_name "$release_name" '[.[] | select(.name==$release_name) | {id: .id, assets_url: .assets_url, name: .name, assets: .assets}]'  <<< "$releases_complete")

echo "releases filtered: $releases"

# if this release doesn't exist, create it and upload the asset.
length=$(jq '. | length' <<< "$releases")
echo "$length"

if [ $length == "0" ];
then
  create_release
else
  if [ $length == "1" ];
  then
    replace_asset 
  else
    echo "More than one release found. Not sure what to do. Exiting." 
  fi
fi



release_id=$(jq '.[] | .id' <<< "$releases")
assets_url=$(jq '.[] | .assets_url' <<< "$releases")

#echo "$releases"
echo "$assets_url"
echo "$release_id"

# find the asset_id for the asset we are replacing.


# delete the asset we care about replacing.


# upload the replacement asset.
#uploaded=$(curl \
#    -u nvogel:"$GITHUB_TOKEN"
#    -X POST 
#    -H "Accept: application/vnd.github.v3+json"
#    -H "Content-Type: application.octet-stream"
    

