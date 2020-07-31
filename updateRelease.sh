#!/bin/bash


# note - to run using env vars in the running shell,run with `. .updateRelease`
release_name=uiExplorerData
asset_name=unemployment_data.parquet
owner=clsphila
repository=ui-data-explorer

echo "Updating release $release_name"

function upload_asset {
  release_id=$1
  echo "Uploading asset to $release_id"
  uploaded_response=$(curl \
    -X POST \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept:application/vnd.github.v3+json" \
    -H "Content-Type:application/octet-stream" \
    --data-binary @"data/unemployment_data.parquet" \
    "https://uploads.github.com/repos/$owner/$repository/releases/$release_id/assets?name=$asset_name"
    )
  echo "finished uploading asset."
}

function create_release {
  echo "Release not found. Creating."
  new_release=$(curl \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
     "https://api.github.com/repos/$owner/$repository/releases" \
    -d "{\"tag_name\":\"$release_name\", \"name\":\"$release_name\"}"
    )
  echo "new release: $new_release"
  release_id=$(jq '.id' <<< $new_release)
  echo "Assets url: $assets_url"
  echo "end of assets url"
  upload_asset $release_id
}

function clear_assets {
  $assets=$1
  for row in $(echo "${assets}" | jq -r '.[] | @base64'); do
    _jq() {
     echo ${row} | base64 --decode | jq -r ${1}
    }
    asset_id="$(_jq '.id')"
    echo "Delete Asset: $asset_id"
    deleted=$(curl \
    -X DELETE \
    -u natev:"$GITHUB_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
     "https://api.github.com/repos/$owner/$repository/releases/assets/$asset_id"
    )
  done 
}

function replace_asset {
  echo "Replacing asset."
  release=$1
  release_id=$(jq '.id' <<< "$release")
  assets=$(jq '.assets' <<< "$release")
  asset_count=$(jq '. | length'  <<< "$assets")  
  if [ $asset_count != "0" ]; then
    echo "need to delete assets."
    clear_assets $assets
  fi

  assets_url=$(jq '.assets_url' <<< "$release")
  release_id=$(jq '.id' <<< "$release")
  upload_asset $release_id
}

# get the releases
releases_complete=$(curl \
  -H "Accept: application/vnd.github.v3+json" \
  https://api.github.com/repos/clsphila/ui-data-explorer/releases)


# find the release we care about with its name.
releases=$(jq --arg release_name "$release_name" '[.[] | select(.name==$release_name) | {id: .id, upload_url: .upload_url, assets_url: .assets_url, name: .name, assets: .assets}]'  <<< "$releases_complete")


# if this release doesn't exist, create it and upload the asset.
length=$(jq '. | length' <<< "$releases")

if [ $length == "0" ];
then
  create_release
else
  if [ $length == "1" ];
  then
    release=$(jq '.[0]' <<< "$releases")
    replace_asset "$release"
  else
    echo "More than one release named $release_name found. Not sure what to do. Exiting." 
  fi
fi



   

